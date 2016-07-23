module FrayTracer.Core.Light

open FSharp.Data.UnitSystems.SI.UnitSymbols

module Speed =
    let vacuum = 299792458.0<m/s>

    let ofRefractionIndex (x:float) =
        vacuum / x

module Colors =
    ()    
