[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module FrayTracer.SdfMaterial

let createSolid (color) : SdfMaterial =
    {
        Color = fun _ -> color
    }

let inline getColor (position) (material:SdfMaterial) =
    material.Color position
