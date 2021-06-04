namespace FrayTracer

open System.Numerics
open System

type Undefined = unit

type Material =
    {
    IntensityCoeff : float32
    RefractionIndex : float32
    Intensity : float32
    }
