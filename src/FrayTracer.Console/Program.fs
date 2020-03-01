module FrayTracer.Console.Program

open FrayTracer
open System.Diagnostics

let shellOpen (path) =
    Process.Start(ProcessStartInfo(path, UseShellExecute = true ))

////////////////////////////////
// MAIN

// 19 36 78 86
Random.setSeed 86

let camera =
    Camera.lookAt {
        Position = Vector3(0.0f, 0.0f, -10.0f)
        LookAt = Vector3(0.0f, 0.0f, 0.0f)
        Up = Vector3.UnitY
        Lens = Lens.create 60.0f
    }

let size = 1000

let imageSize =
    {
    X = size
    Y = size
    }

let randomSphere () =
    SDF.Primitive.sphere {
        Center = Random.pointInBall () * 5.0f
        Radius = Random.uniform 0.3f 1.0f
    }
        //(Material.surface (Random.uniform 0.4f 0.8f))

let sdf =
    List.init 50 (fun _ -> randomSphere ())
    |> List.reduce (SDF.Combine.unionSmooth 6.0f)
    |> FrayTracer.SDF.Test.compile

let timer = Stopwatch.StartNew()

let traced =
    sdf
    |> SDF.Test.traceWithDirectionalLigth 0.01f Vector3.UnitZ
    |> Image.render imageSize camera

timer.Stop()
printfn "Time = %A" timer.Elapsed

traced
|> Image.normalize
//|> Image.gamma 2.2f
|> Image.dither (1.0f / 256.0f)
|> Image.saveBitmap "test"
|> shellOpen
|> ignore
