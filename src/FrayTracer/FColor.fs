namespace FrayTracer

open System
open System.Numerics
open System.Drawing

[<Struct>]
type FColor =
    | FColor of Vector3

    member this.R = let (FColor color) = this in color.X
    member this.G = let (FColor color) = this in color.Y
    member this.B = let (FColor color) = this in color.Z

    static member (+) (FColor left, FColor right) : FColor =
        left + right
        |> FColor

    static member (*) (FColor left, FColor right) : FColor =
        left * right
        |> FColor

    static member (*) (FColor left, right:float32) : FColor =
        left * right
        |> FColor

    static member (/) (FColor left, right:float32) : FColor =
        left / right
        |> FColor

module FColor =
    let ofRGB (r) (g) (b) : FColor =
        Vector3(r, g, b)
        |> FColor

    let ofColor (color:Color) : FColor =
        Vector3(
            float32 color.R / 255f,
            float32 color.G / 255f,
            float32 color.B / 255f
        ) |> FColor

    let toColor (rng:Random) (FColor color) : Color =
        Color.FromArgb(
            color.X * 255f + rng.range_01 () |> MathF.floor_i,
            color.Y * 255f + rng.range_01 () |> MathF.floor_i,
            color.Z * 255f + rng.range_01 () |> MathF.floor_i
        )

    let gammaInverse (gamma:float32) (FColor color) =
        Vector3(
            MathF.Pow(color.X, gamma),
            MathF.Pow(color.Y, gamma),
            MathF.Pow(color.Z, gamma)
        ) |> FColor        

    let getMaxColor (FColor color) : float32 =
        Vector3.maxDimension color
