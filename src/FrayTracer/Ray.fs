[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module FrayTracer.Ray

open System.Numerics

let inline get (length:float32) (ray:Ray) =
    ray.Origin + ray.Direction * length

let inline move (length:float32) (ray:Ray) =
    {ray with
        Origin = ray |> get length
        Length = ray.Length - length
    }

let inline setDirection (direction:Vector3) (ray:Ray) =
    {ray with Direction = direction}

let inline toFastDistanceQuery (ray:Ray) : SdfFastDistanceQuery =
    {
        Position = ray.Origin
        Epsilon = ray.Epsilon * 2f
    }
