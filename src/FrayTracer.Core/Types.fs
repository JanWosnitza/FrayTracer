namespace FrayTracer.Core

open System.Numerics
open FSharp.Data.UnitSystems.SI.UnitSymbols

[<StructuredFormatDisplay( "{StructuredFormatDisplay}" )>]
type Material = {
    Name : string
    RefractionIndex : float32
    Color : Vector3 * float<Hz> -> float32
    Glow : Vector3 * float<Hz> -> float32
} with

    override this.ToString() = this.Name
    member private this.StructuredFormatDisplay = this.ToString()

type RayHit = {
    Distance : float32<m>
    Normal : Vector3
    Material : Material
}

type Ray = {
    Position : Vector3
    Direction : Vector3
    Frequency : float<Hz>
    Time : float32<s>
    Coeff : float32
    Intensity : float32
}

type Volume = Ray -> RayHit option
type Surface = RayHit -> RayHit

[<Measure>] type degC
type Camera = {
    Position : Vector3
    LookAt : Vector3
    Up  : Vector3
    Fov : float<degC> 
}
