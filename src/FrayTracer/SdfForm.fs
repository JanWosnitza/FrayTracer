[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module FrayTracer.SdfForm

open System
open System.Numerics

let createTrace (boundary) (distance) =
    let trace (ray) =
        match SdfBoundary.trace boundary ray with
        | Miss -> Single.PositiveInfinity
        | Inside -> distance ray.Origin
        | Hit length -> length

    trace

let tryDistance (sdf:SdfForm) (position:Vector3) =
    if SdfBoundary.isInside sdf.Boundary position then
        sdf.Distance position
        |> ValueSome
    else
        ValueNone

let union (sdfs:seq<SdfForm>) =
    match sdfs |> Seq.toArray with
    | [||] -> failwith "No SdfObjects given."
    | [|sdf|] -> sdf
    | sdfs ->
        let boundary =
            sdfs
            |> Seq.map (fun x -> x.Boundary)
            |> SdfBoundary.unionMany

        sdfs
        |> Array.sortInPlaceBy (fun sdf -> SdfBoundary.getMinDistance sdf.Boundary boundary.Center)

        {
            Distance =
                fun (position) ->
                let mutable min = Single.PositiveInfinity

                for i = 0 to sdfs.Length - 1 do
                    let sdf = sdfs.[i]
                    if min > SdfBoundary.getMinDistance sdf.Boundary position then
                        min <- sdf.Distance(position) |> MathF.min min
                min
            Boundary = boundary
            Trace =
                fun (ray) ->
                if not <| SdfBoundary.traceTest boundary ray then Single.PositiveInfinity else

                let mutable min = Single.PositiveInfinity
                for i = 0 to sdfs.Length - 1 do
                    let sdf = sdfs.[i]
                    if min > SdfBoundary.getMinDistance sdf.Boundary ray.Origin then
                        min <- sdf.Trace(ray) |> MathF.min min
                min
        }

let subtraction (a:SdfForm) (sdfs:seq<SdfForm>) =
    match sdfs |> Seq.toArray with
    | [||] -> a
    | sdfs ->
        let distance (position) =
            let mutable max = a.Distance position
            for i = 0 to sdfs.Length - 1 do
                let obj = sdfs.[i]
                if max < -SdfBoundary.getMinDistance obj.Boundary position then
                    max <- -(obj.Distance position) |> MathF.max max
            max

        let boundary = a.Boundary

        {
            Distance = distance
            Boundary = boundary
            Trace = createTrace boundary distance
        }

let intersection (sdfs:seq<SdfForm>) =
    match sdfs |> Seq.toArray with
    | [||] -> failwith "No SdfObjects given."
    | [|sdf|] -> sdf
    | sdfs ->
        let distance (position) =
            let mutable max = Single.NegativeInfinity
            for i = 0 to sdfs.Length - 1 do
                let obj = sdfs.[i]
                if max < SdfBoundary.getMaxDistance obj.Boundary position then
                    max <- obj.Distance position |> MathF.max max
            max

        let boundary = sdfs |> Seq.map (fun x -> x.Boundary) |> SdfBoundary.intersectionMany

        {
            Distance = distance
            Boundary = boundary
            Trace = createTrace boundary distance
        }

let unionTree (sdfs:seq<SdfForm>) =
    match sdfs |> Seq.toList with
    | [] -> failwith "No SdfObjects given."
    | [sdf] -> sdf
    | sdfs ->
        // combine objects in an octree like fashion

        let boxMin, boxSize =
            let min = sdfs |> Seq.map (fun x -> x.Boundary.Center - Vector3(x.Boundary.Radius)) |> Seq.reduce Vector3.min
            let max = sdfs |> Seq.map (fun x -> x.Boundary.Center + Vector3(x.Boundary.Radius)) |> Seq.reduce Vector3.max
            let size = max - min |> Vector3.maxDimension

            let center = Vector3.Lerp(min, max, 0.5f)

            center - Vector3(size * 0.5f),  size

        let toInt (x:float32) = x / boxSize * float32 Int32.MaxValue |> MathF.round |> uint

        let rec loop (mask:uint) (sdfs:list<SdfForm>) =
            match sdfs with
            | [] -> failwith "unreachable"
            | [sdf] -> sdf
            | sdfs ->
                // fallback, due to possible floating point inprecision
                if mask = UInt32.MaxValue then union sdfs else

                let small, big = sdfs |> List.partition (fun o -> toInt o.Boundary.Radius <= mask)

                let unioned =
                    small
                    |> List.groupBy (fun x ->
                        let p = x.Boundary.Center - boxMin
                        let mask = ~~~mask
                        struct (toInt p.X &&& mask, toInt p.Y &&& mask, toInt p.Z &&& mask)
                    )
                    |> List.map (fun (_, sdfs) -> union sdfs)

                loop ((mask <<< 1) + 1u) (unioned @ big)

        let mask =
            sdfs
            |> Seq.map (fun x -> x.Boundary.Radius)
            |> Seq.min
            |> toInt
            |> Bits.toPowerOf2Minus1
            |> max 1u

        loop mask sdfs

let unionSmooth (strength:float32) (sdfs:seq<SdfForm>) =
    match sdfs |> Seq.toArray with
    | [||] -> failwithf "blub"
    | [|sdf|] -> sdf
    | sdfs ->
        let strengthInverse = -1f / strength

        let distance (position) =
            let mutable sum = 0f
            for i = 0 to sdfs.Length - 1 do
                let distance = sdfs.[i].Distance position
                sum <- sum + MathF.Exp(strengthInverse * distance)

            -MathF.Log(sum) * strength

        let boundary =
            sdfs
            |> Seq.map (fun x -> x.Boundary)
            |> SdfBoundary.unionMany
        {
            Distance = distance
            Boundary = boundary
            Trace = createTrace boundary distance
        }

module Primitive =
    [<Struct>]
    type Sphere =
        {
            Center : Vector3
            Radius : float32
        }

    let sphere (data:Sphere) =
        let distance (position) = Vector3.Distance(data.Center, position) - data.Radius
        let boundary : SdfBoundary = {Center = data.Center; Radius = data.Radius}
        {
            Distance = distance
            Boundary = boundary
            Trace =
                fun ray ->
                match SdfBoundary.trace boundary ray with
                | Miss -> Single.PositiveInfinity
                | Inside _ -> distance ray.Origin
                | Hit length -> length
        }

    [<Struct>]
    type Capsule =
        {
            From : Vector3
            To : Vector3
            Radius : float32
        }

    let capsule (data:Capsule) =
        let dir = (data.To - data.From) |> Vector3.normalize

        let distance (position) =
            let distance =
                let diff = position - data.From
                let t = Vector3.Dot(diff, dir)
                if t <= 0f then
                    diff.Length()
                elif t >= 1f then
                    Vector3.Distance(diff, dir)
                else
                    Vector3.Distance(diff, dir * t)

                //Vector3.Distance(diff, dir * MathF.clamp01 t)
                
            distance - data.Radius

        let boundary :SdfBoundary = {
            Center = Vector3.Lerp(data.From, data.To, 0.5f)
            Radius = data.Radius + Vector3.Distance(data.From, data.To) * 0.5f
        }

        {
            Distance = distance
            Boundary = boundary
            Trace = createTrace boundary distance
        }

    [<Struct>]
    type Torus =
        {
            Center : Vector3
            Normal : Vector3
            MajorRadius : float32
            MinorRadius : float32
        }

    let torus (data:Torus) =
        let normal = data.Normal |> Vector3.normalize
        let planeD = -Vector3.dot data.Center normal

        let distance (position) =
            let distanceToPlane = Vector3.Dot(position, normal) + planeD
            let distanceToCenter = Vector3.Distance(data.Center, position - (distanceToPlane * normal))
            let distanceToCircle = distanceToCenter - data.MajorRadius

            Vector2(distanceToPlane, distanceToCircle).Length() - data.MinorRadius

        let boundary :SdfBoundary = {Center = data.Center; Radius = data.MajorRadius + data.MinorRadius}

        {
            Distance = distance
            Boundary = boundary
            Trace = createTrace boundary distance
        }

    [<Struct>]
    type Triangle =
        {
            v1 : Vector3
            v2 : Vector3
            v3 : Vector3
            Radius : float32
        }

    let triangle (data:Triangle) =
        // optimized version of https://iquilezles.org/www/articles/triangledistance/triangledistance.htm
        let v21 = data.v2 - data.v1
        let v21' = v21 |> Vector3.inverseLength
        let v32 = data.v3 - data.v2
        let v32' = v32 |> Vector3.inverseLength
        let v13 = data.v1 - data.v3
        let v13' = v13 |> Vector3.inverseLength
        let nor = Vector3.Cross(v21, v13) |> Vector3.normalize
        let n21 = Vector3.Cross(v21, nor) |> Vector3.normalize
        let n32 = Vector3.Cross(v32, nor) |> Vector3.normalize
        let n13 = Vector3.Cross(v13, nor) |> Vector3.normalize

        let distance (position) =
            let p1 = position - data.v1
            let p2 = position - data.v2
            let p3 = position - data.v3

            let distance =
                // inside/outside test
                if
                    ((Vector3.dot n21 p1 |> MathF.sign_i)
                    + (Vector3.dot n32 p2 |> MathF.sign_i)
                    + (Vector3.dot n13 p3 |> MathF.sign_i)) < 2
                then
                    // 3 edges
                    let d21 = p1 |> Vector3.dot v21' |> MathF.clamp01 |> Vector3.scale v21 |> Vector3.distance2 p1
                    let d32 = p2 |> Vector3.dot v32' |> MathF.clamp01 |> Vector3.scale v32 |> Vector3.distance2 p2
                    let d13 = p3 |> Vector3.dot v13' |> MathF.clamp01 |> Vector3.scale v13 |> Vector3.distance2 p3
                    (d21 |> MathF.min d32 |> MathF.min d13)
                    |> MathF.sqrt
                else
                    // 1 face
                    Vector3.Dot(nor, p1)
                    |> MathF.abs

            distance - data.Radius

        let boundary :SdfBoundary =
            {
            Center =
                let areaInv = 0.5f / Vector3.Cross(data.v1 - data.v2, data.v2 - data.v3).LengthSquared()
                let w1 = (data.v2 - data.v3).LengthSquared() * Vector3.Dot(data.v1 - data.v2, data.v1 - data.v3) * areaInv
                let w2 = (data.v1 - data.v3).LengthSquared() * Vector3.Dot(data.v2 - data.v1, data.v2 - data.v3) * areaInv
                let w3 = 1f - w1 - w2 // (data.v1 - data.v2).LengthSquared() * Vector3.Dot(data.v3 - data.v1, data.v3 - data.v2) * areaInv
                w1 * data.v1 + w2 * data.v2 + w3 * data.v3
            Radius =
                v21.Length() * v32.Length() * v13.Length() / 2f / Vector3.Cross(v21,v32).Length()
                + data.Radius
            }

        {
            Distance = distance
            Boundary = boundary
            Trace = createTrace boundary distance
        }
