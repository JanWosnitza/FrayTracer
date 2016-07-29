namespace FrayTracer.Core

open System.Numerics
open FSharp.Data.UnitSystems.SI.UnitSymbols

type Config = {
    // TODO
    Fresnel : float32 -> unit
    }

[<StructuredFormatDisplay( "{StructuredFormatDisplay}" )>]
type Material = {
    Name : string
    RefractionIndex : float32
    Color : Vector3 * float<Hz> -> float32
    Glow : Vector3 * float<Hz> -> float32
    } with

    override this.ToString() = this.Name
    member private this.StructuredFormatDisplay = this.ToString()

type LightRay = {
    Ray : Ray
    Frequency : float<Hz>
    Time : float32<s>
    Coeff : float32
    Intensity : float32
    }

[<Measure>] type degC
type Camera = {
    Position : Vector3
    LookAt : Vector3
    Up  : Vector3
    Fov : float<degC> 
    }
