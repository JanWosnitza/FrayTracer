module FrayTracer.Test

open System
open System.Numerics

let rec trace (sdf:SdfForm) (length:float32) (ray:Ray) =
    if length < 0f then
        ValueNone
    else
        let distance = sdf.Trace ray
        if distance < ray.Epsilon then
            ValueSome (ray.Origin, distance)
        else
            trace sdf (length - distance) (ray |> Ray.move distance)

let normal (sdf:SdfForm) (epsilon:float32) (position:Vector3) =
    let inline f (dimension:Vector3) =
        let epsilon = dimension * epsilon
        sdf.Distance (position + epsilon) - sdf.Distance (position - epsilon)

    Vector3(f Vector3.UnitX, f Vector3.UnitY, f Vector3.UnitZ)
    |> Vector3.normalize

let normalFast (sdf:SdfForm) (epsilon:float32) (position:Vector3) (distanceAtPosition:float32) =
    Vector3(
        sdf.Distance (position + Vector3.UnitX * epsilon) - distanceAtPosition,
        sdf.Distance (position + Vector3.UnitY * epsilon) - distanceAtPosition,
        sdf.Distance (position + Vector3.UnitZ * epsilon) - distanceAtPosition
    )
    |> Vector3.normalize

let traceWithDirectionalLigth (epsilon:float32) (length:float32) (backgroundColor:FColor) (color:FColor) (lightDirection:Vector3) (sdf:SdfForm) =
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

        color * (0.1f + light * 0.9f)
    | ValueNone -> backgroundColor
