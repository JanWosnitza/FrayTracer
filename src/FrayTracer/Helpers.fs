[<AutoOpen>]
module FrayTracer.Helpers

open System
open System.Numerics

[<RequireQualifiedAccess>]
module Math =
    let pi = float32 Math.PI
    let pi2 = pi * 2.0f

    let degToRad = pi / 180.0f
    let radToDeg = 180.0f / pi

    let sqrt2 = sqrt 2.0f

[<RequireQualifiedAccess>]
module Vector3 =
    let inline create (x) (y) (z) = Vector3(x, y, z)

    let inline dot (v1) (v2) = Vector3.Dot(v1, v2)

    let inline cross (v1) (v2) = Vector3.Cross(v1, v2)

    let inline normalize (v) = Vector3.Normalize(v)

    let inline scale (x:float32) (v:Vector3) = Vector3.Multiply(v, x)

let inline vector3 (x) (y) (z) = Vector3.create x y z

[<RequireQualifiedAccess>]
module Random =
    let private rnd = new Random()

    let uniform_01 () = rnd.NextDouble() |> float32

    let uniform_11 () = -1.0f + uniform_01 () * 2.0f

    let uniform (min) (max) = min + uniform_01 () * (max - min)

    let inline vector3 (f) =
        Vector3.create (f ()) (f ()) (f ())

    let normalN11 () =
        // https://en.wikipedia.org/wiki/Box%E2%80%93Muller_transform

        let u1 = uniform_01 () + Single.Epsilon
        let u2 = uniform_01 ()

        let a = sqrt (-2.0f * log u1)
        let b = Math.pi2 * u2

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

module Array2D =
    let toSeqIndexed (array:_[,]) =
        seq {
        for i = 0 to array.GetLength(0) - 1 do
            for j = 0 to array.GetLength(1) - 1 do
                yield (i, j), array.[i, j]
        }

    let ofArrayArray (array:_[][]) =
        let xSize = Array.length array
        let ySize = array |> Seq.map Array.length |> Seq.min

        Array2D.init xSize ySize (fun x y -> array.[x].[y])

    let toSeq (array:_[,]) =
        seq {
        for i = 0 to array.GetLength(0) - 1 do
            for j = 0 to array.GetLength(1) - 1 do
                yield array.[i, j]
        }

    let max (array:_[,]) =
        array
        |> toSeq
        |> Seq.max
