namespace FrayTracer

open System.Numerics

[<Struct>]
type Ray =
    {
        Origin : Vector3
        /// <summary>Normalized direction of the ray.</summary>
        Direction : Vector3
        Length : float32
        Epsilon : float32
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


[<Struct>]
type SdfFastDistanceQuery =
    {
        Position : Vector3
        Threshold : float32
    }

[<Struct>]
type SdfForm =
    {
        Distance : Vector3 -> float32
        Boundary : SdfBoundary
        FastDistance : SdfFastDistanceQuery -> float32
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
        Ray : Ray
        Normal : Vector3
        Color : FColor
    }

    member this.Position = this.Ray.Origin

type SdfScene =
    {
        Object : SdfObject
        BackgroundColor : FColor
        LightDirection : Vector3
    }
