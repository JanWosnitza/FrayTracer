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
    Intensity : float32
    }

[<Struct>]
type RayCastResult =
    {
    Length : float32
    Normal : Vector3
    Material : Material
    }

type Scene = Ray -> RayCastResult option
