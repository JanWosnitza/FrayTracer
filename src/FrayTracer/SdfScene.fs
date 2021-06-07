[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module FrayTracer.SdfScene

open System
open System.Numerics

let trace (scene:SdfScene) (ray:Ray) =
    match  SdfObject.tryTrace scene.Object ray with
    | ValueNone -> scene.BackgroundColor
    | ValueSome result ->
        let light =
            let dot = Vector3.dot result.Normal scene.LightDirection

            if dot >= 0f then
                0f
            else
                // shadow
                {
                    Origin = result.Position + result.Normal * ray.Epsilon
                    Epsilon = ray.Epsilon
                    Direction = -scene.LightDirection
                    Length = result.Ray.Length
                }
                |> SdfForm.tryTrace scene.Object.Form
                |> function
                    | ValueNone -> -dot
                    | ValueSome _ -> 0.0f

        result.Color * (0.1f + light * 0.9f)
