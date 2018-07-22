[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module FrayTracer.Core.Material

let surface (intensityCoeff) =
    {
    IntensityCoeff = intensityCoeff
    Intensity = 0.0f
    }

let light (intensity) =
    {
    IntensityCoeff = 0.0f
    Intensity = intensity
    }
