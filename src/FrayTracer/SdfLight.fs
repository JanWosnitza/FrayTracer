[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module FrayTracer.SdfLight

open System.Numerics

let directional (direction:Direction) (color:FColor) =
    let direction = -direction |> Vector3.normalize
    {
        Direction = fun _ -> direction
        Intensity = fun o r ->
            {
                Origin = r.Origin
                Direction = direction
                Length = 1000f // TODO
                Epsilon = r.Epsilon
            }
            |> SdfObject.tryTrace o
            |> function
                | ValueNone -> color |> ValueSome
                | _ -> ValueNone
    }

let point (position:Position) (color:FColor) =
    {
        Direction = fun p -> (position - p) |> Vector3.normalize
        Intensity = fun o r ->
            let diff = position - r.Origin
            let distance2 = Vector3.length2 diff
            let distance = MathF.sqrt distance2
            let direction = diff / distance2
            
            {
                Origin = r.Origin
                Direction = direction
                Length = distance
                Epsilon = r.Epsilon
            }
            |> SdfObject.tryTrace o
            |> function
                | ValueNone -> color / distance2 |> ValueSome
                | _ -> ValueNone
    }
