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
    for x = 0 to array.GetLength(0) - 1 do
        for y = 0 to array.GetLength(1) - 1 do
            yield array.[x, y]
    }

let max (array:_[,]) =
    array
    |> toSeq
    |> Seq.max

module Parallel =
    let init (length1) (length2) (f) =
        let arrayArray =
            Array.Parallel.init length1 (fun x ->
                Array.init length2 (fun y ->
                    f x y
                )
            )

        Array2D.init length1 length2 (fun x y -> arrayArray.[x].[y])

    let map (mapper) (array:_[,]) =
        init (array.GetLength(0)) (array.GetLength(1)) (fun x y -> mapper array.[x,y])

    let maxWith (mapper) (array:_[,]) =
        let yIndeices = seq { 0 .. array.GetLength(1) - 1 }
        Array.Parallel.init (array.GetLength(0)) (fun x ->
            yIndeices
            |> Seq.map (fun y -> mapper array.[x,y])
            |> Seq.max
        )
        |> Array.max
