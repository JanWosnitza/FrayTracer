namespace FrayTracer.Core

open System.Numerics
open FSharp.Data.UnitSystems.SI.UnitSymbols

type Ray =
    {
    Position : Vector3
    Direction : Vector3
    Frequency : float<Hz>
    Time : float<s>
    }

type Reflectance = Ray -> float

type Material =
    {
    SpeedOfLight : float<m/s>
    Filter : Reflectance
    }

type Surface =
    {
    Enter : Ray -> float<W>
    }

type Volume =
    {
    Test : Ray -> (Vector3 * Vector3) option
    }

// Ray -> Volumetest -> hits Surface -> hits Material
