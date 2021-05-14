namespace FrayTracer

open System.Numerics
open System

type Undefined = unit

[<Struct>]
type Ray =
    {
    Position : Vector3
    Direction : Vector3
    }

type Material =
    {
    IntensityCoeff : float32
    RefractionIndex : float32
    Intensity : float32
    }
