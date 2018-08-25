[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module FrayTracer.Material

let surface (intensityCoeff) =
    {
    IntensityCoeff = intensityCoeff
    RefractionIndex = 1.0f
    Intensity = 0.0f
    }

let light (intensity) =
    {
    IntensityCoeff = 0.0f
    RefractionIndex = 1.0f
    Intensity = intensity
    }
