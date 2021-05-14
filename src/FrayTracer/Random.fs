[<RequireQualifiedAccess>]
module FrayTracer.Random

open System

let mutable private rnd = Random()

let setSeed (seed) = rnd <- Random(seed)

let uniform_01 () = rnd.NextDouble() |> float32

let uniform_11 () = -1.0f + uniform_01 () * 2.0f

let uniform (min) (max) = min + uniform_01 () * (max - min)

let inline private vector3 (f) =
    Vector3(f (), f (), f ())

let normalN11 () =
    // https://en.wikipedia.org/wiki/Box%E2%80%93Muller_transform

    let u1 = uniform_01 () + Single.Epsilon
    let u2 = uniform_01 ()

    let a = sqrt (-2.0f * log u1)
    let b = MathF.pi2 * u2

    let z0 = a * cos b
    //let z1 = a * sin b
        
    z0

let rec pointInBall () =
    let vec = vector3 uniform_11
    if vec.LengthSquared() <= 1.0f
        then vec
        else pointInBall ()

let rec pointOnSphere () =
    let vec = vector3 uniform_11
    let len = vec.LengthSquared()
    if (0.01f <= len) && (len <= 1.0f)
        then vec / sqrt len
        else pointOnSphere ()
