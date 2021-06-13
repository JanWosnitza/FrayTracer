[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module FrayTracer.SdfForm

open System
open System.Numerics

let tryDistance (sdf:SdfForm) (position:Vector3) =
    if SdfBoundary.isInside sdf.Boundary position then
        sdf.Distance position
        |> ValueSome
    else
        ValueNone

let union (forms:seq<SdfForm>) =
    match forms |> Seq.toArray with
    | [||] -> failwith "No SdfObjects given."
    | [|form|] -> form
    | forms ->
        {
            Distance =
                let getForms = forms |> SdfBoundary.buildSpatialLookup (fun x -> x.Boundary)
                fun (position) ->
                let forms = getForms position

                let mutable min = Single.PositiveInfinity
                for i = 0 to forms.Length - 1 do
                    let sdf = forms.[i]
                    if min > SdfBoundary.getMinDistance sdf.Boundary position then
                        min <- sdf.Distance(position) |> MathF.min min
                min

            Boundary =
                forms
                |> Seq.map (fun x -> x.Boundary)
                |> SdfBoundary.unionMany
        }

let subtract (a:SdfForm) (b:SdfForm) =
    {
        Distance =
            fun position ->
            a.Distance position
            |> MathF.max -(b.Distance position)
        Boundary = a.Boundary
    }

let intersect (forms:seq<SdfForm>) =
    match forms |> Seq.toArray with
    | [||] -> failwith "No SdfObjects given."
    | [|form|] -> form
    | forms ->
        {
            Distance =
                fun position ->
                let mutable max = Single.NegativeInfinity
                for i = 0 to forms.Length - 1 do
                    let obj = forms.[i]
                    if max < SdfBoundary.getMaxDistance obj.Boundary position then
                        max <- obj.Distance position |> MathF.max max
                max
            Boundary = forms |> Seq.map (fun x -> x.Boundary) |> SdfBoundary.intersectionMany
        }

let unionSmooth (strength:float32) (forms:seq<SdfForm>) =
    match forms |> Seq.toArray with
    | [||] -> failwithf "blub"
    | [|sdf|] -> sdf
    | sdfs ->
        {
            Distance =
                let strengthInverse = -1f / strength
                fun position ->
                let mutable sum = 0f
                for i = 0 to sdfs.Length - 1 do
                    let distance = sdfs.[i].Distance position
                    sum <- sum + MathF.Exp(strengthInverse * distance)

                -MathF.Log(sum) * strength

            Boundary =
                sdfs
                |> Seq.map (fun x -> x.Boundary)
                |> SdfBoundary.unionMany
        }

let rec tryTrace (sdf:SdfForm) (ray:Ray) : voption<SdfFormTraceResult> =
    if ray.Length <= 0f then
        ValueNone
    else
        let distance = ray.Origin |> sdf.Distance
        if distance < ray.Epsilon then
            ValueSome {
                Ray = ray
                Distance = distance
            }
        else
            tryTrace sdf (ray |> Ray.move distance)

let normal (sdf:SdfForm) (epsilon:float32) (position:Vector3, distance:float32) =
    (*let inline f (dimension:Vector3) =
        let epsilon = dimension * epsilon
        sdf.Distance (position + epsilon) - sdf.Distance (position - epsilon)

    Vector3(f Vector3.UnitX, f Vector3.UnitY, f Vector3.UnitZ)
    |> Vector3.normalize*)

    //let distance = sdf.Distance position

    (Vector3(
        sdf.Distance (Vector3(position.X + epsilon, position.Y, position.Z)),
        sdf.Distance (Vector3(position.X, position.Y + epsilon, position.Z)),
        sdf.Distance (Vector3(position.X, position.Y, position.Z + epsilon))
     ) - Vector3(distance))
    |> Vector3.normalize

module Primitive =
    [<Struct>]
    type Sphere =
        {
            Center : Vector3
            Radius : float32
        }

    let sphere (data:Sphere) =
        {
            Distance =
                fun position ->
                Vector3.Distance(data.Center, position) - data.Radius

            Boundary = {
                Center = data.Center
                Radius = data.Radius
            }
        }

    [<Struct>]
    type Capsule =
        {
            From : Vector3
            To : Vector3
            Radius : float32
        }

    let capsule (data:Capsule) =
        {
            Distance =
                let dir = (data.To - data.From) |> Vector3.normalize

                fun position ->
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

            Boundary = {
                Center = Vector3.Lerp(data.From, data.To, 0.5f)
                Radius = data.Radius + Vector3.Distance(data.From, data.To) * 0.5f
            }
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
        let data =
            {data with
                Normal = data.Normal |> Vector3.normalize
            }

        {
            Distance =
                let planeD = -(Vector3.dot data.Center data.Normal)
                fun position ->
                let distanceToPlane = Vector3.Dot(position, data.Normal) + planeD
                let distanceToCenter = Vector3.Distance(data.Center, position - (distanceToPlane * data.Normal))
                let distanceToCircle = distanceToCenter - data.MajorRadius

                Vector2(distanceToPlane, distanceToCircle).Length() - data.MinorRadius

            Boundary = {
                Center = data.Center
                Radius = data.MajorRadius + data.MinorRadius
            }
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

        let inline distance (position) =
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
        }
