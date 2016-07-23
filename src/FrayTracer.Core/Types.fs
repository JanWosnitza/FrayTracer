namespace FrayTracer.Core

open System.Numerics
open FSharp.Data.UnitSystems.SI.UnitSymbols

[<StructuredFormatDisplay( "{StructuredFormatDisplay}" )>]
type Material =
    {
    Name : string
    RefractionIndex : float32
    Reflectance : float<Hz> -> float32
    }

    override this.ToString() = this.Name
    member private this.StructuredFormatDisplay = this.ToString()

type HitType =
    | Enter
    | Leave

type RayHit =
    {
    Distance : float32
    Normal : Vector3
    Material : Material
    }

type Ray =
    {
    Position : Vector3
    Direction : Vector3
    Frequency : float<Hz>
    Time : float<s>
    Coeff : float32
    }

type Volume = Ray -> RayHit option
type Surface = RayHit -> RayHit

[<Measure>] type degC
type Camera =
    {
    Position : Vector3
    LookAt : Vector3
    Up  : Vector3
    Fov : float<degC> 
    }
// Ray -> Volumetest -> hits Surface -> hits Material
