[<CompilationRepresentation( CompilationRepresentationFlags.ModuleSuffix )>]
module FrayTracer.Core.Ray

open System.Numerics

let reflect (hit:RayHit) (ray:Ray) =
    {ray with
        Position = ray.Position + ray.Direction * (hit.Distance - 0.001f)
        Direction = Vector3.Reflect( ray.Direction, hit.Normal )
        Coeff = ray.Coeff * (hit.Material.Reflectance ray.Frequency)
    }
