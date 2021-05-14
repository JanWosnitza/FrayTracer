namespace FrayTracer

open System

type Vector2 = System.Numerics.Vector2
type Vector3 = System.Numerics.Vector3

[<RequireQualifiedAccess>]
module MathF =
    let pi = float32 Math.PI
    let pi2 = float32 (Math.PI * 2.0)

    let sqrt2 = sqrt 2.0f
    let sqrt3 = sqrt 3.0f

    let inline degToRad (x) = x * pi / 180.0f
    let inline radToDeg (x) = x * 180.0f / pi

    let inline min (min:float32) (x:float32) =
        MathF.Min(min, x)

    let inline max (max:float32) (x:float32) =
        MathF.Max(max, x)

    let inline clamp (min:float32) (max:float32) (x:float32) =
        MathF.Max(min, MathF.Min(max, x))

    let inline roundToInt (x:float32) = MathF.Round(x) |> int

[<RequireQualifiedAccess>]
module Vector3 =
    let inline dot (v1) (v2) = Vector3.Dot(v1, v2)

    let inline cross (v1) (v2) = Vector3.Cross(v1, v2)

    let inline normalized (v) = Vector3.Normalize(v)

    let inline scale (x:float32) (v:Vector3) = Vector3.Multiply(v, x)
