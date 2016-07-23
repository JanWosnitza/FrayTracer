[<CompilationRepresentation( CompilationRepresentationFlags.ModuleSuffix )>]
module FrayTracer.Core.Material

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

// RefractionIndex: http://www.rpi.edu/dept/phys/Dept2/APPhys1/optics/optics/node4.html
// TODO Reflectance
let vacuum = {
    Name = "Vacuum"
    RefractionIndex = 1.0f
    Reflectance = fun (_) -> 0.0f
}

let glass = {
    Name = "Glass"
    RefractionIndex = 1.52f
    Reflectance = fun (_) -> 0.2f
}

let air = {
    Name = "Air"
    RefractionIndex = 1.0003f
    Reflectance = fun (_) -> 0.0f
}

let water = {
    Name = "Water"
    RefractionIndex = 1.33f
    Reflectance = fun (_) -> 0.0f
}

let salt = {
    Name = "Salt"
    RefractionIndex = 1.54f
    Reflectance = fun (_) -> 0.0f
}

let asphalt = {
    Name = "Asphalt"
    RefractionIndex = 1.635f
    Reflectance = fun (_) -> 0.0f
}

let heavyFlintGlass = {
    Name = "Heavy Flint Glass"
    RefractionIndex = 1.65f
    Reflectance = fun (_) -> 0.0f
}

let diamond = {
    Name = "Diamond"
    RefractionIndex = 2.42f
    Reflectance = fun (_) -> 0.0f
}

let Lead = {
    Name = "Lead"
    RefractionIndex = 2.6f
    Reflectance = fun (_) -> 0.0f
}
