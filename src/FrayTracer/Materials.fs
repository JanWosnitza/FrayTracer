module FrayTracer.Materials

// RefractionIndex:
// • http://www.rpi.edu/dept/phys/Dept2/APPhys1/optics/optics/node4.html
// • https://en.wikipedia.org/wiki/Refractive_index#Typical_values

let vacuum = {
    IntensityCoeff = 1.0f
    Intensity = 0.0f
    RefractionIndex = 1.0f
}

let glass = {
    IntensityCoeff = 1.0f
    Intensity = 0.0f
    RefractionIndex = 1.52f
}

let air = {
    IntensityCoeff = 1.0f
    Intensity = 0.0f
    RefractionIndex = 1.0003f
}

let water = {
    IntensityCoeff = 1.0f
    Intensity = 0.0f
    RefractionIndex = 1.33f
}

let salt = {
    IntensityCoeff = 1.0f
    Intensity = 0.0f
    RefractionIndex = 1.54f
}

let asphalt = {
    IntensityCoeff = 1.0f
    Intensity = 0.0f
    RefractionIndex = 1.635f
}

let heavyFlintGlass = {
    IntensityCoeff = 1.0f
    Intensity = 0.0f
    RefractionIndex = 1.65f
}

let diamond = {
    IntensityCoeff = 1.0f
    Intensity = 0.0f
    RefractionIndex = 2.42f
}

let lead = {
    IntensityCoeff = 1.0f
    Intensity = 0.0f
    RefractionIndex = 2.6f
}
