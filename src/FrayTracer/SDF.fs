namespace FrayTracer.SDF

open System
open FrayTracer

type ISignedDistanceField =
    abstract GetDistance : position:Vector3 -> float32

module Primitive =
    type Sphere =
        {
            Center : Vector3
            Radius : float32
        }

        interface ISignedDistanceField with
            member this.GetDistance(position) =
                Vector3.Distance(this.Center, position) - this.Radius

    let sphere (x:Sphere) = x  :> ISignedDistanceField

module Combine =
    let union (a:ISignedDistanceField) (b:ISignedDistanceField) =
        {new ISignedDistanceField with
            member this.GetDistance(position) =
                MathF.Min(
                    a.GetDistance(position),
                    b.GetDistance(position)
                )
        }

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

    let unionSmooth (strength:float32) (sdfs:ISignedDistanceField list) =
        match sdfs with
        | [] -> failwithf "blub"
        | [sdf] -> sdf
        | _ ->
            let sdfs = sdfs |> List.toArray

            {new ISignedDistanceField with
                member this.GetDistance(position) =
                    let mutable sum = 0f
                    for i = 0 to sdfs.Length - 1 do
                        sum <- sum + MathF.Exp(-strength * sdfs.[i].GetDistance(position))

                    -MathF.Log(sum) / strength
            }

    let cache (width:float32) (epsilon:float32) (sdf:ISignedDistanceField) =
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

module Test =
    let trace (epsilon:float32) (length:float32) (sdf:ISignedDistanceField) (ray:Ray) =
        let direction = ray.Direction
        let position = ray.Position

        let rec test (position:Vector3) (length:float32) =
            let distance = sdf.GetDistance(position)
            if distance < epsilon then
                ValueSome (position, distance)
            elif length <= 0f then
                ValueNone
            else
                test (position + direction * distance) (length - distance)

        let distance = sdf.GetDistance(position)
        test (position + direction * distance) length

    let normal (epsilon:float32) (sdf:ISignedDistanceField) (position:Vector3) =
        let inline f (dimension:Vector3) =
            let epsilon = dimension * epsilon
            sdf.GetDistance(position + epsilon) - sdf.GetDistance(position - epsilon)

        Vector3(f Vector3.UnitX, f Vector3.UnitY, f Vector3.UnitZ)
        |> Vector3.normalized

    let normalFAST (epsilon:float32) (sdf:ISignedDistanceField) (position:Vector3) (distanceAtPosition:float32) =
        Vector3(
            sdf.GetDistance(position + Vector3.UnitX * epsilon) - distanceAtPosition,
            sdf.GetDistance(position + Vector3.UnitY * epsilon) - distanceAtPosition,
            sdf.GetDistance(position + Vector3.UnitZ * epsilon) - distanceAtPosition
        )
        |> Vector3.normalized

    let traceWithDirectionalLigth (epsilon:float32) (length:float32) (sdf:ISignedDistanceField) (lightDirection:Vector3) =
        let inline trace (ray) = trace epsilon length sdf ray
        let normalEpsilon = epsilon / 1.0f
        let inline normal (position) (distance) = normalFAST normalEpsilon sdf position distance

        let lightDirection = -lightDirection
        fun (ray:Ray) ->
        match  trace ray with
        | ValueSome (position, distance) ->
            let normal = normal position distance
            let light =
                let dot = Vector3.dot normal lightDirection

                if dot <= 0f then
                    0f
                else
                    // shadow
                    trace {
                        Position = position + normal * epsilon
                        Direction = lightDirection
                    } |> function
                    | ValueNone -> dot
                    | ValueSome _ -> 0.0f

            0.1f + light * 0.9f
        | ValueNone -> 0.0f
