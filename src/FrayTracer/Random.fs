//[<RequireQualifiedAccess>]
[<AutoOpen>]
module FrayTracer.Random

open System

type Random with
    member rng.range_01 () = rng.NextDouble() |> float32

    member rng.range (min) (max) = min + rng.range_01 () * (max - min)

    member rng.normalN11 () =
        // https://en.wikipedia.org/wiki/Box%E2%80%93Muller_transform

        let u1 = rng.range_01 () + Single.Epsilon
        let u2 = rng.range_01 ()

        let a = sqrt (-2.0f * log u1)
        let b = MathF.pi2 * u2

        let z0 = a * cos b
        //let z1 = a * sin b
        
        z0

    member rng.pointInBall (radius:float32) =
        let vec = Vector3(rng.range -1f 1f, rng.range -1f 1f, rng.range -1f 1f)
        if vec.LengthSquared() <= 1.0f then
            vec * radius
        else
            rng.pointInBall radius

    member rng.pointOnSphere (radius:float32) =
        let vec = Vector3(rng.range -1f 1f, rng.range -1f 1f, rng.range -1f 1f)
        let len = vec.LengthSquared()
        if (0.01f <= len) && (len <= 1.0f) then
            vec / sqrt len * radius
        else
            rng.pointOnSphere radius
