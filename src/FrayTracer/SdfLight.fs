[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module FrayTracer.SdfLight

open System.Numerics

let directional (direction:Direction) (color:FColor) =
    let direction = -direction |> Vector3.normalize
    {
        Direction = fun _ -> direction
        Color = fun _ -> color
    }

let point (position:Position) (color:FColor) =
    {
        Direction = fun p -> (position - p) |> Vector3.normalize
        Color = fun p -> color / Vector3.distance2 position p
    }
