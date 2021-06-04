[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module FrayTracer.SdfBoundary

open System
open System.Numerics

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
