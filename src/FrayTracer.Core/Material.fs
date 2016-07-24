[<CompilationRepresentation( CompilationRepresentationFlags.ModuleSuffix )>]
module FrayTracer.Core.Material

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

let private noGlow (_) = 0.0f

// RefractionIndex: http://www.rpi.edu/dept/phys/Dept2/APPhys1/optics/optics/node4.html
// TODO Color
let vacuum = {
    Name = "Vacuum"
    RefractionIndex = 1.0f
    Color = fun (_) -> 0.0f
    Glow = noGlow
}

let glass = {
    Name = "Glass"
    RefractionIndex = 1.52f
    Color = fun (_) -> 0.01f
    Glow = noGlow
}

let air = {
    Name = "Air"
    RefractionIndex = 1.0003f
    Color = fun (_) -> 0.0f
    Glow = noGlow
}

let water = {
    Name = "Water"
    RefractionIndex = 1.33f
    Color = fun (_) -> 0.0f
    Glow = noGlow
}

let salt = {
    Name = "Salt"
    RefractionIndex = 1.54f
    Color = fun (_) -> 0.0f
    Glow = noGlow
}

let asphalt = {
    Name = "Asphalt"
    RefractionIndex = 1.635f
    Color = fun (_) -> 0.0f
    Glow = noGlow
}

let heavyFlintGlass = {
    Name = "Heavy Flint Glass"
    RefractionIndex = 1.65f
    Color = fun (_) -> 0.0f
    Glow = noGlow
}

let diamond = {
    Name = "Diamond"
    RefractionIndex = 2.42f
    Color = fun (_) -> 0.0f
    Glow = noGlow
}

let Lead = {
    Name = "Lead"
    RefractionIndex = 2.6f
    Color = fun (_) -> 0.0f
    Glow = noGlow
}
