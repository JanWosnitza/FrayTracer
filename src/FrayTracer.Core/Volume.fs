[<CompilationRepresentation( CompilationRepresentationFlags.ModuleSuffix )>]
module FrayTracer.Core.Volume

open System.Numerics

let move (offset:Vector3) (volume:Volume) (ray:Ray) =
    volume {ray with Position = ray.Position + offset}
