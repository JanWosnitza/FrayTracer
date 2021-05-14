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

let sdf1 =
    List.init 50 (fun _ -> randomSphere ())
    |> SDF.Combine.unionSmooth 6.0f
    //|> List.reduce SDF.Combine.union

//let sdf2 = {new SDF.ISignedDistanceField with member this.GetDistance(position: Vector3): float32 = position.Y - position.X}

let sdf3 =
    let a =
        SDF.Primitive.sphere {
            Center = Vector3(-1.f, 0.f, 0.f)
            Radius = 1.f
        }
    let b =
        SDF.Primitive.sphere {
            Center = Vector3(1.f,0.f,0.f)
            Radius = 2.f
        }
    SDF.Combine.unionSmooth 10.f [a; b]

let sdf = sdf1
//printfn "%A" (sdf.GetType())

let timer = Stopwatch.StartNew()
let traced =
    sdf
    |> SDF.Combine.cache 0.1f 0.01f
    |> SDF.Test.traceWithDirectionalLigth 0.01f 1000f Vector3.UnitZ
    |> Image.render imageSize camera
timer.Stop()

printfn "Time    = %10.0f ms" timer.Elapsed.TotalMilliseconds

traced
|> Image.normalize
|> Image.gamma 2.2f
|> Image.dither (1.0f / 256.0f)
|> Image.saveBitmap "test"
|> shellOpen
|> ignore
