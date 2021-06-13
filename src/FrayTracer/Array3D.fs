module Array3D

module Parallel =
    let init (length1) (length2) (length3) (f) =
        let arrayArray =
            Array.init length1 (fun x ->
                Array.Parallel.init length2 (fun y ->
                    Array.init length3 (fun z ->
                        f x y z
                    )
                )
            )

        Array3D.init length1 length2 length3 (fun x y z -> arrayArray.[x].[y].[z])
