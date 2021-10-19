[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module FrayTracer.SdfScene

open System
open System.Numerics

let trace (scene:SdfScene) =
    fun (ray:Ray) ->
    match  SdfObject.tryTrace scene.Object ray with
    | ValueNone -> scene.BackgroundColor
    | ValueSome result ->
        let mutable lightColor = scene.BackgroundColor
        for light in scene.Lights do
            let lightDirection = light.Direction result.Position
            let lightCos = Vector3.dot result.Normal lightDirection

            if lightCos > 0f then
                result.Ray
                |> light.Intensity scene.Object
                |> function
                    | ValueSome intensity ->
                        // no shadow
                        lightColor <- lightColor + intensity * lightCos
                    | ValueNone ->
                        // shadow
                        ()

        result.Color * (lightColor * MathF.piInv)
