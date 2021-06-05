[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module FrayTracer.SdfScene

open System
open System.Numerics

let trace (epsilon:float32) (length:float32) (scene:SdfScene) (ray:Ray) =
    match  SdfObject.tryTrace scene.Object length ray with
    | ValueNone -> scene.BackgroundColor
    | ValueSome result ->
        let light =
            let dot = Vector3.dot result.Normal scene.LightDirection

            if dot >= 0f then
                0f
            else
                // shadow
                {
                    Origin = result.Position + result.Normal * epsilon
                    Epsilon = epsilon
                    Direction = -scene.LightDirection
                }
                |> SdfForm.tryTrace scene.Object.Form length
                |> function
                    | ValueNone -> -dot
                    | ValueSome _ -> 0.0f

        result.Color * (0.1f + light * 0.9f)
