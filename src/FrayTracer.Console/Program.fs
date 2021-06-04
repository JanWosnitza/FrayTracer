﻿module FrayTracer.Console.Program

open FrayTracer
open System.Diagnostics
open System.Numerics

let shellOpen (path) =
    Process.Start(ProcessStartInfo(path, UseShellExecute = true ))

////////////////////////////////
// MAIN

// 19 36 78 86
let rng = System.Random(78) //86

let camera =
    Camera.lookAt {
        Position = Vector3(0.0f, 0.0f, -10.0f)
        LookAt = Vector3(0.0f, 0.0f, 0.0f)
        Up = Vector3.UnitY
        Lens = Lens.create 60.0f
    }

let size = 1000

let imageSize = {X = size; Y = size}

let randomSphere () =
    SdfForm.Primitive.sphere {
        Center = rng.pointInBall 4.0f
        Radius = rng.range 0.3f 1.0f
    }

let randomCapsule () =
    let center = rng.pointInBall 4.0f
    SdfForm.Primitive.capsule {
        From = center
        To = center + rng.pointOnSphere (rng.range 0.5f 2.0f)
        Radius = rng.range 0.1f 0.3f
    }

let randomTorus () =
    SdfForm.Primitive.torus {
        Center = rng.pointInBall 4.0f
        Normal = rng.pointOnSphere 1f
        MajorRadius = rng.range 0.1f 0.4f
        MinorRadius = rng.range 0.1f 0.3f
    }

let randomTriangle () =
    let v1 = rng.pointInBall 4.0f
    SdfForm.Primitive.triangle {
        v1 = v1
        v2 = v1 + rng.pointOnSphere (rng.range 0.5f 2.0f)
        v3 = v1 + rng.pointOnSphere (rng.range 0.5f 2.0f)
        Radius = rng.range 0.1f 0.5f
    }

let sdf1 =
    SdfForm.subtraction
        (SdfForm.intersection [
            SdfForm.union [
                for i = 1 to 100 do yield randomTorus ()
            ]

            //SDF.Primitive.sphere {Center = Vector3(0f,0f,0f); Radius = 5f}
        ])
        [
            //SDF.Primitive.sphere {Center = Vector3(0f,1f,-2f); Radius = 3f}
        ]

let lightDir = (0f, -1f, 1f) |> Vector3 |> Vector3.normalize
let epsilon = 0.01f
let backgroundColor = FColor.ofRGB 0f 0f 0f
let objectColor = FColor.ofRGB 0f 0f 1f

let sdf =
    sdf1
    //|> SDF.Performance.cache 0.1f epsilon
//printfn "%A" (sdf.GetType())

printfn $"Rendering..."

let timer = Stopwatch.StartNew()
let traced =
    sdf
    |> Test.traceWithDirectionalLigth epsilon 1000f backgroundColor objectColor lightDir
    |> Image.render imageSize camera epsilon
timer.Stop()

printfn $"Time = {timer.Elapsed.TotalSeconds:F1} sec"

traced
|> Image.normalize
|> Image.gamma 2.2f
|> Image.toColors rng
|> Image.saveBitmap "test"
|> shellOpen
|> ignore
