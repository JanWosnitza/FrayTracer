[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module FrayTracer.SdfMaterial

let createSolid (color) : SdfMaterial =
    {
        Color = fun _ -> color
    }

let inline getColor (material:SdfMaterial) (position) =
    material.Color position
