module FrayTracer.Console.Program

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
        v1 = v1
        v2 = v1 + rng.pointOnSphere (rng.range 0.5f 2.0f)
        v3 = v1 + rng.pointOnSphere (rng.range 0.5f 2.0f)
        Radius = rng.range 0.1f 0.5f
    }
    |> SdfObject.create (randomMaterial ())

let scene =
    {
        Object =
            SdfObject.subtraction
                (SdfObject.intersection
                    (SdfObject.union [
                        for i = 1 to 1000 do yield randomTorus ()
                    ])
                    [SdfForm.Primitive.sphere {Center = Vector3(0f,0f,0f); Radius = 3.5f}]
                )
                [SdfForm.Primitive.sphere {Center = Vector3(-0.5f,1f,-2f); Radius = 2f}]
        LightDirection = (-0.5f, -1f, 1f) |> Vector3 |> Vector3.normalize
        BackgroundColor = FColor.ofRGB 0f 0f 0f
    }

let epsilon = 0.01f

printfn $"Rendering..."

let timer = Stopwatch.StartNew()
let traced =
    scene
    |> SdfScene.trace epsilon 1000f
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
