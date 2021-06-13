[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module FrayTracer.SdfBoundary

open System
open System.Numerics


let createFastDistanceQuery (getDistance:Vector3->float32) (boundary:SdfBoundary) =
    let fastDistance (query:SdfFastDistanceQuery) =
#if false
        let boxDistance =
            (
#if !false
                (boundary.Center.X - query.Position.X |> MathF.abs)
                |> MathF.max (boundary.Center.Y - query.Position.Y |> MathF.abs)
                |> MathF.max (boundary.Center.Z - query.Position.Z |> MathF.abs)
#else
                (boundary.Center - query.Position)
                |> Vector3.abs
                |> Vector3.maxDimension
#endif
            ) - boundary.Radius

        if boxDistance > query.Threshold * 2f then boxDistance else
#endif

        let sphereDistance = Vector3.Distance(boundary.Center, query.Position) - boundary.Radius
        if sphereDistance > query.Threshold then
            sphereDistance
        else
            getDistance query.Position

    fastDistance

let createFastDistanceQuery2 (getDistance:SdfFastDistanceQuery->float32) (boundary:SdfBoundary) =
    let fastDistance (query:SdfFastDistanceQuery) =
        let sphereDistance = Vector3.Distance(boundary.Center, query.Position) - boundary.Radius
        if sphereDistance > query.Threshold then
            sphereDistance
        else
            getDistance query

    fastDistance

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
    |> Seq.reduce intersection

let isInside (x:SdfBoundary) (position:Vector3) = Vector3.DistanceSquared(x.Center, position) < x.Radius * x.Radius

let getMinMaxDistance (x:SdfBoundary) (position:Vector3) =
    let distance = Vector3.Distance(x.Center, position)
    struct (distance - x.Radius, distance + x.Radius)

let getMinDistance (x:SdfBoundary) (position:Vector3) = Vector3.Distance(x.Center, position) - x.Radius
let getMaxDistance (x:SdfBoundary) (position:Vector3) = Vector3.Distance(x.Center, position) + x.Radius

module AABB =
    let getMin (boundary:SdfBoundary) = boundary.Center - Vector3(boundary.Radius)
    let getMax (boundary:SdfBoundary) = boundary.Center + Vector3(boundary.Radius)

    let trace (boundary : SdfBoundary) (ray : Ray) =
        let irdir = Vector3.One / ray.Direction
        let diff = boundary.Center - ray.Origin

        let radius = Vector3(boundary.Radius)
        let a = (diff - radius) * irdir
        let b = (diff + radius) * irdir

        let min = Vector3.min a b |> Vector3.maxDimension
        let max = Vector3.max a b |> Vector3.minDimension

        // box missed
        if min > max then SdfBoundaryTraceResult.Miss else

        // behind box
        if max < -ray.Epsilon then SdfBoundaryTraceResult.Miss else

        if min <= ray.Epsilon then SdfBoundaryTraceResult.Inside else

        SdfBoundaryTraceResult.Hit min

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

        let t = MathF.Sqrt(b2 - c)

        // is behind spehere?
        if b >= t then false else

        // longer than ray
        -b - t < ray.Length

    let trace (boundary : SdfBoundary) (ray : Ray) =
        let co = ray.Origin - boundary.Center
        let b = Vector3.Dot(co, ray.Direction)

        // more than radius behind sphere, cannot hit or be inside
        if boundary.Radius <= b then SdfBoundaryTraceResult.Miss else
            
        let b2 = b * b
        let c = co.LengthSquared() - boundary.Radius * boundary.Radius

        // missing sphere
        if b2 <= c then
            SdfBoundaryTraceResult.Miss
        else
            let t = MathF.Sqrt(b2 - c)

            // behind sphere
            if b >= t then SdfBoundaryTraceResult.Miss else

            let t1 = -b - t

            // longer than ray
            if t1 > ray.Length then SdfBoundaryTraceResult.Miss else

            // hitting
            if t1 > ray.Epsilon then SdfBoundaryTraceResult.Hit t1 else
                
            // inside
            SdfBoundaryTraceResult.Inside
    //*)

//open Sphere
//let inline traceTest (boundary : SdfBoundary) (ray : Ray) = traceTest boundary ray
//let inline trace (boundary : SdfBoundary) (ray : Ray) = trace boundary ray

let buildSpatialLookup (getBoundary:'T -> SdfBoundary) (items:'T[]) : Vector3 -> 'T[]=
    // very simple spatial distribution. can take some time to initialize :(
    let boundaries = items |> Seq.map (fun x -> getBoundary x)

    let aabbMin = boundaries |> Seq.map AABB.getMin |> Seq.reduce Vector3.min
    let aabbMax = boundaries |> Seq.map AABB.getMax |> Seq.reduce Vector3.max
    let countSize = 1.5f * (boundaries |> Seq.map (fun x -> x.Radius) |> Seq.average)

    let aabbSize = aabbMax - aabbMin

    let countX = aabbSize.X / countSize |> MathF.ceiling_i |> max 1
    let countY = aabbSize.X / countSize |> MathF.ceiling_i |> max 1
    let countZ = aabbSize.X / countSize |> MathF.ceiling_i |> max 1
    let cellSize = aabbSize / Vector3(float32 countX, float32 countY, float32 countZ)
    let cellSizeInv = Vector3(1f) / cellSize

    let cells = Array3D.Parallel.init countX countY countZ (fun x y z ->
        let center = aabbMin + cellSize * 0.5f + cellSize * Vector3(float32 x, float32 y, float32 z)

        let upperBound =
            (boundaries
            |> Seq.map (fun x -> getMaxDistance x center)
            |> Seq.min)
            + (cellSize * 0.5f).Length()

        let items' =
            items
            |> Array.where (fun x -> getMinDistance (getBoundary x) center < upperBound)

        // make checking nearest SDF first most likely
        items'
        |> Array.sortInPlaceBy (fun x -> getMinDistance (getBoundary x) center)

        items'
    )

    fun (position) ->
    let c = (position - aabbMin) * cellSizeInv
    cells.[
        MathF.floor_i c.X |> MathI.clamp 0 (countX - 1),
        MathF.floor_i c.Y |> MathI.clamp 0 (countY - 1),
        MathF.floor_i c.Z |> MathI.clamp 0 (countZ - 1)
    ]
