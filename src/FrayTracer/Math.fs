namespace FrayTracer

open System
open System.Numerics

[<RequireQualifiedAccess>]
module Bits =
    let inline toPowerOf2Minus1 (x) =
        let inline f (shift) (x) = x ||| (x >>> shift)
        x
        |> f 1
        |> f 2
        |> f 4
        |> f 8
        |> f 16

    //let toPowerOf2 (x:int) = toPowerOf2Minus1 (x - 1) + 1

[<RequireQualifiedAccess>]
module MathI =
    let inline min (min:int) (x:int) = Math.Min(min, x)
    let inline max (max:int) (x:int) = Math.Max(max, x)
    let inline clamp (min:int) (max:int) (x:int) = Math.Max(min, Math.Min(max, x))

[<RequireQualifiedAccess>]
module MathF =
    let pi = MathF.PI
    let pi2 = MathF.PI * 2.0f

    let sqrt (x:float32) = sqrt x

    let [<Literal>] sqrt2 = 1.414213562f // sqrt 2.0f
    let [<Literal>] sqrt3 = 1.732050808f // sqrt 3.0f

    let inline degToRad (x) = x * 0.01745329252f
    let inline radToDeg (x) = x * 57.29577951f

    let inline sign_i (x:float32) = MathF.Sign(x)

    let inline sign (x:float32) = MathF.Sign(x) |> float32

    let inline abs (x:float32) = MathF.Abs(x)

    let inline min (min:float32) (x:float32) = MathF.Min(min, x)

    let inline max (max:float32) (x:float32) = MathF.Max(max, x)

    let inline clamp (min:float32) (max:float32) (x:float32) = MathF.Max(min, MathF.Min(max, x))
    let inline clamp01 (x:float32) = MathF.Max(0f, MathF.Min(1f, x))

    let inline floor (x:float32) = MathF.Floor(x)
    let inline round (x:float32) = MathF.Round(x)
    let inline ceiling (x:float32) = MathF.Ceiling(x)

    let inline floor_i (x:float32) = MathF.Floor(x) |> int
    let inline round_i (x:float32) = MathF.Round(x) |> int
    let inline ceiling_i (x:float32) = MathF.Ceiling(x) |> int

[<RequireQualifiedAccess>]
module Vector3 =
    let inline length (v:Vector3) = v.Length()
    let inline length2 (v:Vector3) = v.LengthSquared()

    let inline distance (v1:Vector3) (v2:Vector3) = Vector3.Distance(v1, v2)
    let inline distance2 (v1:Vector3) (v2:Vector3) = Vector3.DistanceSquared(v1, v2)

    let inline inverseLength (v:Vector3) = v / v.LengthSquared()

    let inline scale (v:Vector3) (scale:float32) = v * scale

    let inline dot (v1) (v2) = Vector3.Dot(v1, v2)

    let inline normalize (v) = Vector3.Normalize(v)

    let inline min (v1) (v2) = Vector3.Min(v1, v2)
    let inline max (v1) (v2) = Vector3.Max(v1, v2)

    let inline abs (v) = Vector3.Abs(v)

    let inline minDimension (v:Vector3) = v.X |> MathF.min v.Y |> MathF.min v.Z
    let inline maxDimension (v:Vector3) = v.X |> MathF.max v.Y |> MathF.max v.Z
