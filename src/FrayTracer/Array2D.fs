[<RequireQualifiedAccess>]
module FrayTracer.Array2D

let toSeqIndexed (array:_[,]) =
    seq {
    for i = 0 to array.GetLength(0) - 1 do
        for j = 0 to array.GetLength(1) - 1 do
            yield (i, j), array.[i, j]
    }

let ofArrayArray (array:_[][]) =
    let xSize = Array.length array
    let ySize = array |> Seq.map Array.length |> Seq.min

    Array2D.init xSize ySize (fun x y -> array.[x].[y])

let toSeq (array:_[,]) =
    seq {
    for y = 0 to array.GetLength(1) - 1 do
        for x = 0 to array.GetLength(0) - 1 do
            yield array.[x, y]
    }

let max (array:_[,]) =
    array
    |> toSeq
    |> Seq.max

module Parallel =
    let init (length1) (length2) (f) =
        Array.init length1 (fun x -> Array.Parallel.init length2 (f x))
        |> ofArrayArray
