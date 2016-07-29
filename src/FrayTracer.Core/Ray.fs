[<CompilationRepresentation( CompilationRepresentationFlags.ModuleSuffix )>]
module FrayTracer.Core.Ray

open System.Numerics
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

(*
let start (frequency) (position) (direction) =
    {
    LightRay.Position = position
    Direction = direction
    Frequency = frequency
    Time = 0.f<s>
    Coeff = 1.0f
    Intensity = 0.f
    }
*)    

(*let reflect (hit:RayHit) (ray:Ray) =
    let position = ray.Position + ray.Direction * float32 (hit.Distance - 0.001f<m>)
    {ray with
        Position = position
        Direction = Vector3.Reflect( ray.Direction, hit.Normal )
        Coeff = ray.Coeff * (hit.Material.Reflectance (position, ray.Frequency))
        Intensity = ray.Intensity + ray.Coeff * hit.Material.Glow (position, ray.Frequency)
        Time = ray.Time + hit.Distance / (Light.Speed.ofRefractionIndex hit.Material.RefractionIndex)
    }
*)
//let refract (hit:RayHit) (ray:Ray) =
