namespace FrayTracer

open System.Numerics

[<Struct>]
type Ray =
    {
        Epsilon : float32
        Origin : Vector3
        /// <summary>Normalized direction of the ray.</summary>
        Direction : Vector3
    }

[<Struct>]
type SdfBoundary =
    {
        Center : Vector3
        Radius : float32
    }

[<Struct>]
type SdfBoundaryTraceResult =
    | Miss
    | Hit of EnterDistance:float32
    | Inside

type SdfForm =
    {
        Distance : Vector3 -> float32
        Boundary : SdfBoundary
        Trace : Ray -> float32
    }

[<Struct>]
type SdfFormTraceResult =
    {
        Position : Vector3
        Distance : float32
    }

type SdfMaterial =
    {
        Color : Vector3 -> FColor
    }

type SdfObject =
    {
        Form : SdfForm
        Material : SdfMaterial
    }

[<Struct>]
type SdfObjectTraceResult =
    {
        Position : Vector3
        Normal : Vector3
        Color : FColor
    }

type SdfScene =
    {
        Object : SdfObject
        BackgroundColor : FColor
        LightDirection : Vector3
    }
