[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module FrayTracer.SdfMaterial

let createSolid (color) : SdfMaterial =
    {
        Color = fun _ _ -> color
    }

let inline getColor (position) (normal) (material:SdfMaterial) : FColor =
    material.Color position normal
