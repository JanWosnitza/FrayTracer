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
    let union (a : SdfBoundary) (b : SdfBoundary) =
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

    let unionMany (boundaries : seq<SdfBoundary>) =
        // this might results a larger boundary than strictly necessary
        boundaries
        |> Seq.reduce union

    let intersection (a : SdfBoundary) (b : SdfBoundary) =
        let diff = b.Center - a.Center
        let distance = diff.Length()

        if distance + b.Radius <= a.Radius then
            b
        elif distance + a.Radius <= b.Radius then
            a
        else
            let dir = diff / distance
            let a' = a.Center + dir * a.Radius
            let b' = b.Center - dir * b.Radius
            {
                Center = (a' + b') * 0.5f
                Radius =
                    let d2 = distance * distance
                    let aR2 = a.Radius * a.Radius
                    let bR2 = b.Radius * b.Radius
                    // https://mathworld.wolfram.com/Sphere-SphereIntersection.html
                    MathF.Sqrt(4f * d2 * aR2 - (d2 - bR2 + aR2)) / (2f * distance)
            }

    let intersectionMany (boundaries : seq<SdfBoundary>) =
        // this might results a larger boundary than strictly necessary
        boundaries
        |> Seq.reduce union

    let isInside (x:SdfBoundary) (position:Vector3) = Vector3.DistanceSquared(x.Center, position) < x.Radius * x.Radius

    let getMinMaxDistance (x:SdfBoundary) (position:Vector3) =
        let distance = Vector3.Distance(x.Center, position)
        struct (distance - x.Radius, distance + x.Radius)

    let getMinDistance (x:SdfBoundary) (position:Vector3) = Vector3.Distance(x.Center, position) - x.Radius
    let getMaxDistance (x:SdfBoundary) (position:Vector3) = Vector3.Distance(x.Center, position) + x.Radius

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
                if t2 < 0f then TraceResult.Miss else

                let t1 = b - t
                // hitting
                if t1 > ray.Epsilon then TraceResult.Hit t1 else
                
                // inside
                TraceResult.Inside
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

    let tryDistance (sdf:SdfObject) (position:Vector3) =
        if SdfBoundary.isInside sdf.Boundary position then
            sdf.Distance position
            |> ValueSome
        else
            ValueNone

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

module Operator =
    let union (sdfs:seq<SdfObject>) =
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

            let distance (position) =
                let mutable min = Single.PositiveInfinity

                for i = 0 to sdfs.Length - 1 do
                    let sdf = sdfs.[i]
                    if min > SdfBoundary.getMinDistance sdf.Boundary position then
                        min <- sdf.Distance(position) |> MathF.min min
                min

            {
                Distance = distance
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

    let subtraction (a:SdfObject) (sdfs:seq<SdfObject>) =
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
                Trace = SdfObject.createTrace boundary distance
            }

    let intersection (sdfs:seq<SdfObject>) =
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
                Trace = SdfObject.createTrace boundary distance
            }

    let unionTree (sdfs:seq<SdfObject>) =
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

    let unionSmooth (strength:float32) (sdfs:seq<SdfObject>) =
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
                Trace = SdfObject.createTrace boundary distance
            }

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
