namespace FrayTracer.SDF

open System
open FrayTracer
open System.Numerics

[<Struct>]
type SdfBoundary =
    {
        Center : Vector3
        Radius : float32
    }

[<Struct>]
type TraceResult =
    | Miss
    | Hit of EnterDistance:float32
    | Inside

module SdfBoundary =
    let combine (a : SdfBoundary) (b : SdfBoundary) =
        let diff = b.Center - a.Center
        let distance = diff.Length()

        if distance + b.Radius <= a.Radius then
            a
        elif distance + a.Radius <= b.Radius then
            b
        else
            let dir = diff / distance
            let a' = a.Center - dir * a.Radius
            let b' = b.Center + dir * b.Radius
            {
                Center = (a' + b') * 0.5f
                Radius = Vector3.Distance(a', b') * 0.5f
            }

    let combineMany (boundaries : seq<SdfBoundary>) =
        // this might results a larger boundary than strictly necessary
        boundaries
        |> Seq.reduce combine

    module AABB =
        let trace (boundary : SdfBoundary) (ray : Ray) =
            let irdir = Vector3.One / ray.Direction
            let diff = boundary.Center - ray.Origin

            let radius = Vector3(boundary.Radius)
            let a = (diff - radius) * irdir
            let b = (diff + radius) * irdir

            let min = Vector3.min a b |> Vector3.maxDimension
            let max = Vector3.max a b |> Vector3.minDimension

            // box missed
            if min > max then TraceResult.Miss else

            // behind box
            if max < -ray.Epsilon then TraceResult.Miss else

            if min <= ray.Epsilon then TraceResult.Inside else

            TraceResult.Hit min

        let traceTest (boundary : SdfBoundary) (ray : Ray) =
            let irdir = Vector3.One / ray.Direction
            let diff = boundary.Center - ray.Origin
            let radius = Vector3(boundary.Radius)
            let a = (diff - radius) * irdir
            let b = (diff + radius) * irdir

            let min = Vector3.min a b |> Vector3.maxDimension
            let max = Vector3.max a b |> Vector3.minDimension

            // box missed
            (min <= max) && (max >= ray.Epsilon)

    module Sphere =
        (*
        let traceTest (boundary : SdfBoundary) (ray : Ray) =
            let radius2 = boundary.Radius * boundary.Radius

            let diff = boundary.Center - ray.Origin

            let distance2 = diff.LengthSquared()

            if distance2 <= radius2 then
                true
            else
                let diffRayLength = Vector3.Dot(diff, ray.Direction)

                if diffRayLength < -ray.Epsilon then
                    false
                else
                    let shortestDist2 = distance2 - diffRayLength * diffRayLength

                    shortestDist2 <= radius2

        let trace (boundary : SdfBoundary) (ray : Ray) =
            let radius2 = boundary.Radius * boundary.Radius

            let diff = boundary.Center - ray.Origin

            let diffLength2 = diff.LengthSquared()

            // inside sphere
            if diffLength2 <= radius2 then TraceResult.Inside else

            let diffRayLength = Vector3.Dot(diff, ray.Direction)

            // behind sphere
            if diffRayLength < 0f then TraceResult.Miss else

            let shortestDistanceToRay2 = diffLength2 - diffRayLength * diffRayLength

            if shortestDistanceToRay2 > radius2 then TraceResult.Miss else

            let length = diffRayLength - MathF.sqrt (radius2 - shortestDistanceToRay2)
            if length <= ray.Epsilon then
                TraceResult.Inside
            else
                TraceResult.Hit length
        //*)

        //(*
        // optimized version of https://viclw17.github.io/2018/07/16/raytracing-ray-sphere-intersection/
        let traceTest (boundary : SdfBoundary) (ray : Ray) =
            let co = ray.Origin - boundary.Center
            let b = Vector3.Dot(co, ray.Direction)
            
            // more than radius behind sphere, cannot hit or be inside
            if boundary.Radius <= b then false else
            
            let b2 = b * b
            let c = co.LengthSquared() - boundary.Radius * boundary.Radius

            // missing sphere
            if b2 <= c then false else

            // is behind spehere?
            MathF.Sqrt(b2 - c) + ray.Epsilon > b

        let trace (boundary : SdfBoundary) (ray : Ray) =
            let co = ray.Origin - boundary.Center
            let b = Vector3.Dot(co, ray.Direction)

            // more than radius behind sphere, cannot hit or be inside
            if boundary.Radius <= b then TraceResult.Miss else
            
            let b2 = b * b
            let c = co.LengthSquared() - boundary.Radius * boundary.Radius

            // missing sphere
            if b2 <= c then
                TraceResult.Miss
            else
                let t = MathF.Sqrt(b2 - c)
                let b = -b

                let t2 = b + t
                // behind sphere
                if t2 < -ray.Epsilon then TraceResult.Miss else

                let t1 = b - t
                // hitting
                if t1 > ray.Epsilon then TraceResult.Hit t1 else
                
                // inside
                TraceResult.Inside
        //*)

        (* http://kylehalladay.com/blog/tutorial/math/2013/12/24/Ray-Sphere-Intersection.html
        let traceTest (boundary : SdfBoundary) (ray : Ray) =
            let radius2 = boundary.Radius * boundary.Radius

            //solve for tc
            let L = boundary.Center - ray.Origin;
            let tc = Vector3.Dot(L, ray.Direction);
            if tc < 0f then false else

            let d2 = tc * tc - L.LengthSquared()
            d2 < radius2

        let trace (boundary : SdfBoundary) (ray : Ray) =
            let radius2 = boundary.Radius * boundary.Radius

            //solve for tc
            let diff = boundary.Center - ray.Origin
            let diffLength2 = diff.LengthSquared()

            if diffLength2 < radius2 then TraceResult.Inside else

            let tc = Vector3.Dot(diff, ray.Direction)
            if tc < 0f then TraceResult.Miss else

            let d2 = tc * tc - diffLength2

            if d2 > radius2 then TraceResult.Miss else

            //solve for t1c
            let t1c = MathF.Sqrt(radius2 - d2)

            //solve for intersection points
            let t1 = tc - t1c
            if t1 < ray.Epsilon then
                TraceResult.Inside
            else
                TraceResult.Hit t1
        //*)
    
    open Sphere
    let inline traceTest (boundary : SdfBoundary) (ray : Ray) = traceTest boundary ray
    let inline trace (boundary : SdfBoundary) (ray : Ray) = trace boundary ray

type SdfObject =
    {
        Distance : Vector3 -> float32
        Boundary : SdfBoundary
        Trace : Ray -> float32
    }

module SdfObject =
    let createTrace (boundary) (distance) =
        let trace (ray) =
            match SdfBoundary.trace boundary ray with
            | Miss -> Single.PositiveInfinity
            | Inside -> distance ray.Origin
            | Hit length -> length

        trace

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
        let dir = data.To - data.From
        let dirInv = dir / dir.LengthSquared()

        let distance (position) =
            let diff = position - data.From
            let t = Vector3.Dot(diff, dirInv)

            let distance =
                if t <= 0f then
                    diff.Length()
                elif t >= 1f then
                    Vector3.Distance(diff, dir)
                else
                    Vector3.Distance(diff, dir * t)
                
            distance - data.Radius

        let boundary :SdfBoundary = {
            Center = Vector3.Lerp(data.From, data.To, 0.5f)
            Radius = data.Radius + Vector3.Distance(data.From, data.To) * 0.5f
        }

        {
            Distance = distance
            Boundary = boundary
            Trace = SdfObject.createTrace boundary distance
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
            Trace = SdfObject.createTrace boundary distance
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
            Trace = SdfObject.createTrace boundary distance
        }

module Combine =
    (* tree combine
        let rec loop =
            function
            | [|a|] -> a
            | [|a;b|] -> combine a b
            | boundaries ->
                let _as, _bs =
                    boundaries
                    |> Array.splitAt ((boundaries.Length + 1) / 2)
                combine (loop _as) (loop _bs)

        // this might results a larger boundary than strictly necessary
        loop boundaries
    *)

    let unionSimple (sdfs:seq<SdfObject>) =
        match sdfs |> Seq.toArray with
        | [||] -> failwith "No SdfObjects given."
        | [|sdf|] -> sdf
        | sdfs ->
            let boundary =
                sdfs
                |> Seq.map (fun x -> x.Boundary)
                |> SdfBoundary.combineMany

            {
                Distance =
                    fun (position) ->
                    let mutable min = Single.PositiveInfinity
                    for i = 0 to sdfs.Length - 1 do
                        let distance = sdfs.[i].Distance(position)
                        min <- MathF.Min(min, distance)
                    min

                Boundary = boundary
                Trace =
                    fun (ray) ->
                    if not <| SdfBoundary.traceTest boundary ray then
                        Single.PositiveInfinity
                    else
                        let mutable min = Single.PositiveInfinity
                        for i = 0 to sdfs.Length - 1 do
                            let distance = sdfs.[i].Trace(ray)
                            min <- MathF.Min(min, distance)
                        min
            }

    let union (sdfs:seq<SdfObject>) =
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

            let rec loop (mask:uint) (sdfs:list<SdfObject>) =
                match sdfs with
                | [] -> failwith "unreachable"
                | [sdf] -> sdf
                | sdfs ->
                    // fallback, due to possible floating point inprecision
                    if mask = UInt32.MaxValue then unionSimple sdfs else

                    let small, big = sdfs |> List.partition (fun o -> toInt o.Boundary.Radius <= mask)

                    let unioned =
                        small
                        |> List.groupBy (fun x ->
                            let p = x.Boundary.Center - boxMin
                            let mask = ~~~mask
                            struct (toInt p.X &&& mask, toInt p.Y &&& mask, toInt p.Z &&& mask)
                        )
                        |> List.map (fun (_, sdfs) -> unionSimple sdfs)

                    loop ((mask <<< 1) + 1u) (unioned @ big)

            let mask =
                sdfs
                |> Seq.map (fun x -> x.Boundary.Radius)
                |> Seq.min
                |> toInt
                |> Bits.toPowerOf2Minus1
                |> max 1u

            loop mask sdfs

    (*
    let intersection (a:ISignedDistanceField) (b:ISignedDistanceField) =
        {new ISignedDistanceField with
            member this.GetDistance(position) =
                MathF.Max(
                    a.GetDistance(position),
                    b.GetDistance(position)
                )
        }

    let subtraction (a:ISignedDistanceField) (b:ISignedDistanceField) =
        {new ISignedDistanceField with
            member this.GetDistance(position) =
                MathF.Max(
                    a.GetDistance(position),
                    -b.GetDistance(position)
                )
        }

    let unionSmooth (strength:float32) (sdfs:seq<ISignedDistanceField>) =
        match sdfs |> Seq.toArray with
        | [||] -> failwithf "blub"
        | [|sdf|] -> sdf
        | sdfs ->
            let strengthInverse = 1f / strength

            {new ISignedDistanceField with
                member this.GetDistance(position) =
                    let mutable sum = 0f
                    for i = 0 to sdfs.Length - 1 do
                        let distance = sdfs.[i].GetDistance(position)
                        sum <- sum + MathF.Exp(-strengthInverse * distance)

                    -MathF.Log(sum) * strength
            }
    *)

(*module Performance =
    let cache (width:float32) (epsilon:float32) (sdf:SdfObject) =
        let halfDiagonal = sqrt (width * width * 3f) * 0.5f
        let cachedDistances = System.Collections.Concurrent.ConcurrentDictionary<struct (int * int * int), float32>()
        {new ISignedDistanceField with
            member this.GetDistance(position) =
                let width = width
                let x = MathF.roundToInt (position.X / width)
                let y = MathF.roundToInt (position.Y / width)
                let z = MathF.roundToInt (position.Z / width)

                let optDistance =
                    let pos = struct (x, y, z)
                    match cachedDistances.TryGetValue(pos) with
                    | true, optDistance -> optDistance
                    | _ ->
                        let distance =
                            sdf.GetDistance(Vector3(float32 x * width, float32 y * width, float32 z * width))
                            - halfDiagonal
                        
                        let optDistance =
                            if distance > epsilon
                            then distance
                            else Single.NaN
                        cachedDistances.TryAdd(pos, optDistance) |> ignore
                        optDistance

                if Single.IsNaN(optDistance) then
                    sdf.GetDistance(position)
                else
                    optDistance
        }

    let measure (stopwatch:System.Diagnostics.Stopwatch) (sdf:ISignedDistanceField) =
        {new ISignedDistanceField with
            member this.GetDistance(position) =
                stopwatch.Start()
                try
                    sdf.GetDistance(position)
                finally
                    stopwatch.Stop()
        }
*)

module Test =
    let rec trace (sdf:SdfObject) (length:float32) (ray:Ray) =
        if length < 0f then
            ValueNone
        else
            let distance = sdf.Trace ray
            if distance < ray.Epsilon then
                ValueSome (ray.Origin, distance)
            else
                trace sdf (length - distance) (ray |> Ray.move distance)

    let normal (sdf:SdfObject) (epsilon:float32) (position:Vector3) =
        let inline f (dimension:Vector3) =
            let epsilon = dimension * epsilon
            sdf.Distance (position + epsilon) - sdf.Distance (position - epsilon)

        Vector3(f Vector3.UnitX, f Vector3.UnitY, f Vector3.UnitZ)
        |> Vector3.normalize

    let normalFast (sdf:SdfObject) (epsilon:float32) (position:Vector3) (distanceAtPosition:float32) =
        Vector3(
            sdf.Distance (position + Vector3.UnitX * epsilon) - distanceAtPosition,
            sdf.Distance (position + Vector3.UnitY * epsilon) - distanceAtPosition,
            sdf.Distance (position + Vector3.UnitZ * epsilon) - distanceAtPosition
        )
        |> Vector3.normalize

    let traceWithDirectionalLigth (epsilon:float32) (length:float32) (sdf:SdfObject) (lightDirection:Vector3) =
        fun (ray:Ray) ->
        match  trace sdf length ray with
        | ValueSome (position, distance) ->
            let normal = normalFast sdf (ray.Epsilon / 500f) position distance
            let light =
                let dot = Vector3.dot normal lightDirection

                if dot >= 0f then
                    0f
                else
                    // shadow
                    {
                        Origin = position + normal * epsilon
                        Epsilon = epsilon
                        Direction = -lightDirection
                    }
                    |> trace sdf length
                    |> function
                        | ValueNone -> -dot
                        | ValueSome _ -> 0.0f

            0.1f + light * 0.9f
        | ValueNone -> 0.0f
