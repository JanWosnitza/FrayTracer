module FrayTracer.Console.Program

open FrayTracer
open System.Diagnostics

let shellOpen (path) =
    Process.Start(ProcessStartInfo(path, UseShellExecute = true ))

////////////////////////////////
// MAIN

// 19 36 78 86
Random.setSeed 78 //86

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
    SDF.Primitive.sphere {
        Center = Random.pointInBall () * 5.0f
        Radius = Random.uniform 0.3f 1.0f
    }

let randomTorus () =
    SDF.Primitive.torus {
        Center = Random.pointInBall () * 5.0f
        Normal = Random.pointOnSphere ()
        MajorRadius = Random.uniform 0.5f 1.0f
        MinorRadius = Random.uniform 0.1f 0.3f
    }

let sdf1 =
    List.init 50 (fun _ -> randomTorus ())
    |> SDF.Combine.unionSmooth 0.2f

let lightDir = (0f, -1f, 1f) |> Vector3 |> Vector3.normalized
let epsilon = 0.02f

let sdf =
    sdf1
    |> SDF.Performance.cache 0.1f epsilon
//printfn "%A" (sdf.GetType())

printfn $"Rendering..."

let timer = Stopwatch.StartNew()
let traced =
    (sdf, lightDir)
    ||> SDF.Test.traceWithDirectionalLigth  epsilon 1000f
    |> Image.render imageSize camera
timer.Stop()

printfn $"Time = {timer.Elapsed.TotalSeconds:F1} sec"

//System.forms

traced
|> Image.normalize
|> Image.gamma 2.2f
|> Image.dither (1.0f / 256.0f)
|> Image.saveBitmap "test"
|> shellOpen
|> ignore
