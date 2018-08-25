namespace FrayTracer.Core

module Spline =
    let private coefficients = [| [|-0.5; 1.5;-1.5; 0.5|]
                                  [| 1.0;-2.5; 2.0;-0.5|]
                                  [|-0.5; 0.0; 0.5; 0.0|]
                                  [| 0.0; 1.0; 0.0; 0.0|] |]

    let private clamp01 x =
        if   x >= 0.0 && x <= 1.0 then x
        elif x > 1.0 then 1.0 else 0.0 

    let catmulRom1D (knots:array<float>) (x01)=
        let nSpans = knots.Length - 3
        if nSpans < 1 then failwith "catmulRom1D not enough knots!"
        let xClamped = clamp01 x01 * float nSpans
        let span = 
            if   int xClamped >= nSpans then nSpans
            else int xClamped

        let xNorm = xClamped - float span

        let pN = 
            coefficients
            |> Array.map( fun row -> 
                row
                |> Array.mapi( fun i coeff -> coeff * knots.[ (i+span) % knots.Length ] )
                |> Array.sum )

        pN.[1..] |> Array.fold ( fun acc elem -> acc * xNorm + elem ) pN.[0] 
