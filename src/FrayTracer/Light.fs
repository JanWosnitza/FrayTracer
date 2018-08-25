module FrayTracer.Core.Light

open FSharp.Data.UnitSystems.SI.UnitSymbols
open System.Numerics

module Speed =
    let vacuum = 299792458.0f<m/s>

    let ofRefractionIndex (x:float32) =
        vacuum / x

module Colors =
    type Range =
        {Low:float<Hz>; High:float<Hz>}
        member this.Avg = 0.5 * (this.Low + this.High)

    let inline private tHz (x:float) = x * 1.e12<Hz>
    let inline private rangeTHz (l) (h) = {Low = float l |> tHz; High = float h |> tHz}
    
    // https://en.wikipedia.org/wiki/Visible_spectrum#Spectral_colors
    let violet = rangeTHz 668 789
    let blue   = rangeTHz 606 668
    let green  = rangeTHz 526 606
    let yellow = rangeTHz 508 526
    let orange = rangeTHz 484 508
    let red    = rangeTHz 400 484

type Fresnel = {Reflectance:float32; Reflect:Lazy<Vector3>; Transmitance:float32; Transmit:Lazy<Vector3>}

let fresnel (n1) (n2) (normal) (direction) =
    // https://en.wikipedia.org/wiki/Fresnel_equations
    // http://graphics.stanford.edu/courses/cs148-10-summer/docs/2006--degreve--reflection_refraction.pdf
    // http://physics.stackexchange.com/questions/159929/what-does-the-equation-of-a-refracted-ray-trace-mean
    // TODO dispersion https://en.wikipedia.org/wiki/Refractive_index#Complex_refractive_index
    
    let n1n2 = n1 / n2
    let cosi = Vector3.Dot( normal, direction )
    let n1n2cosi = n1n2 * cosi
    let cost = sqrt (1.f + n1n2cosi * cosi - n1n2)

    let rs =
        let a = n2 * cosi
        let b = n1 * cost
        let x = (a - b) / (a + b)
        x * x

    let rp =
        let a = n1 * cosi
        let b = n2 * cost
        let x = (a - b) / (a + b)
        x * x

    let reflectance = 0.5f * (rs + rp)
    {
    Reflectance = reflectance
    Reflect = lazy (direction - 2.f * cosi * normal)
    Transmitance = 1.f - reflectance
    Transmit = lazy (n1n2 * direction + (n1n2cosi - cost) * normal)
    }

// TODO Schlick’s approximation
//let fresnelFast (n1) (n2) (normal) (direction) =
