namespace FrayTracer

open System.Numerics

type Position = Vector3
type Direction = Vector3
type Normal = Vector3

[<Struct>]
type Ray =
    {
        Origin : Position
        /// <summary>Normalized direction of the ray.</summary>
        Direction : Direction
        Length : float32
        Epsilon : float32
    }

[<Struct>]
type SdfBoundary =
    {
        Center : Position
        Radius : float32
    }

[<Struct>]
type SdfBoundaryTraceResult =
    | Miss
    | Hit of EnterDistance:float32
    | Inside

[<Struct>]
type SdfFormTraceResult =
    {
        Ray : Ray
        Distance : float32
    }

//[<Struct>]
type SdfForm =
    {
        Distance : Position -> float32
        Boundary : SdfBoundary
    }

type SdfMaterial =
    {
        Color : Position -> Normal -> FColor
    }

type SdfObject =
    {
        Form : SdfForm
        Material : SdfMaterial
    }

[<Struct>]
type SdfObjectTraceResult =
    {
        Ray : Ray
        Normal : Normal
        Color : FColor
    }

    member this.Position = this.Ray.Origin

type SdfScene =
    {
        Object : SdfObject
        BackgroundColor : FColor
        LightDirection : Direction
        LightColor : FColor
    }
