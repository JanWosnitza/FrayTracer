﻿module FrayTracer.Console.Program

open FrayTracer
open System.Diagnostics
open System.Numerics

let shellOpen (path) =
    Process.Start(ProcessStartInfo(path, UseShellExecute = true ))

////////////////////////////////
// MAIN

// 19 36 78 86
let rng = System.Random(19) //86

let camera =
    Camera.lookAt {
        Position = Vector3(0.0f, 0.0f, -10.0f)
        LookAt = Vector3(0.0f, 0.0f, 0.0f)
        Up = Vector3.UnitY
        Lens = Lens.create 60.0f
    }

let size = 1000

let imageSize = {X = size; Y = size}

let randomMaterial () =
    FColor.ofRGB (rng.range_01 ()) (rng.range_01 ()) (rng.range_01 ())
    |> SdfMaterial.createSolid

let randomSphere () =
    SdfForm.Primitive.sphere {
        Center = rng.pointInBall 4.0f
        Radius = rng.range 0.3f 1.0f
    }
    |> SdfObject.create (randomMaterial ())

let randomCapsule () =
    let center = rng.pointInBall 4.0f
    SdfForm.Primitive.capsule {
        From = center
        To = center + rng.pointOnSphere (rng.range 0.5f 2.0f)
        Radius = rng.range 0.1f 0.3f
    }
    |> SdfObject.create (randomMaterial ())

let randomTorus () =
    SdfForm.Primitive.torus {
        Center = rng.pointInBall 4.0f
        Normal = rng.pointOnSphere 1f
        MajorRadius = rng.range 0.1f 0.4f
        MinorRadius = rng.range 0.1f 0.3f
    }
    |> SdfObject.create (randomMaterial ())

let randomTriangle () =
    let v1 = rng.pointInBall 4.0f
    SdfForm.Primitive.triangle {
        V1 = v1
        V2 = v1 + rng.pointOnSphere (rng.range 0.2f 0.6f)
        V3 = v1 + rng.pointOnSphere (rng.range 0.2f 0.6f)
        Radius = rng.range 0.1f 0.3f
    }
    |> SdfObject.create (randomMaterial ())

let scene =
    {
        Object =
            SdfObject.subtract
                (SdfObject.intersect
                    (SdfObject.union [
                        for i = 1 to 1000 do yield randomTorus ()
                    ])
                    [SdfForm.Primitive.sphere {Center = Vector3(0f,0f,0f); Radius = 3.5f}]
                )
                (SdfForm.Primitive.sphere {Center = Vector3(-0.5f,1f,-2f); Radius = 2.5f})
        BackgroundColor = FColor.ofRGB 0.1f 0.1f 0.1f
        Lights = [
            SdfLight.directional (Vector3(-0.5f, -1f, 1f)) (FColor.ofRGB 0.5f 0.5f 0.5f)
            SdfLight.point (Vector3(-0.5f, 0f, -2f)) ((FColor.ofRGB 10f 0f 0f))
        ]
    }

let epsilon = 0.01f

printfn $"Rendering..."

let timer = Stopwatch.StartNew()
let traced =
    scene
    |> SdfScene.trace
    |> Image.render epsilon 30f imageSize camera
timer.Stop()

printfn $"Time = {timer.Elapsed.TotalSeconds:F2} sec"

traced
|> Image.toColors 2.2f rng
|> Image.saveBitmap "result.bmp"
