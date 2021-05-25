namespace FrayTracer

open System

type Vector2 = System.Numerics.Vector2
type Vector3 = System.Numerics.Vector3

[<RequireQualifiedAccess>]
module MathF =
    let pi = MathF.PI
    let pi2 = MathF.PI * 2.0f

    let sqrt (x:float32) = sqrt x

    let [<Literal>] sqrt2 = 1.414213562f // sqrt 2.0f
    let [<Literal>] sqrt3 = 1.732050808f // sqrt 3.0f

    let inline degToRad (x) = x * 0.01745329252f
    let inline radToDeg (x) = x * 57.29577951f

    let inline signi (x:float32) =
        MathF.Sign(x)

    let inline sign (x:float32) =
        MathF.Sign(x) |> float32

    let inline abs (x:float32) =
        MathF.Abs(x)

    let inline min (min:float32) (x:float32) =
        MathF.Min(min, x)

    let inline max (max:float32) (x:float32) =
        MathF.Max(max, x)

    let inline clamp (min:float32) (max:float32) (x:float32) =
        MathF.Max(min, MathF.Min(max, x))

    let inline roundToInt (x:float32) = MathF.Round(x) |> int

[<RequireQualifiedAccess>]
module Vector3 =
    let inline length (v:Vector3) = v.Length()

    let inline length2 (v:Vector3) = v.LengthSquared()

    let inline dot (v1) (v2) = Vector3.Dot(v1, v2)

    let inline cross (v1) (v2) = Vector3.Cross(v1, v2)

    let inline normalized (v) = Vector3.Normalize(v)

    let inline scale (x:float32) (v:Vector3) = Vector3.Multiply(v, x)
