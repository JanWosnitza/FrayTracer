namespace FrayTracer.Core

open System.Numerics
open System

type Undefined = unit

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

type RayCastResult =
    {
    Length : float32
    Normal : Vector3
    Material : Material
    }

type Scene = Ray -> RayCastResult option
