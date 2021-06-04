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
type TraceResult =
    | Miss
    | Hit of EnterDistance:float32
    | Inside

type SdfForm =
    {
        Distance : Vector3 -> float32
        Boundary : SdfBoundary
        Trace : Ray -> float32
    }
