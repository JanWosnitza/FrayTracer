module FrayTracer.Console.Program

open FrayTracer
open System.Diagnostics
        
let shellOpen (path) =
    Process.Start(ProcessStartInfo(path, UseShellExecute = true ))

////////////////////////////////
// MAIN
 
let camera =
    Camera.ofLookAt {
        Position = vector3 0.0f -10.0f 0.0f
        LookAt = vector3 0.0f 0.0f 0.0f
        Up = vector3 0.0f 0.0f 1.0f
        Lens = Lens.create 60.0f
    }

let tracesPerPixel = 10

let size = 1000

let imageSize =
    {
    SizeX = size
    SizeY = size
    }

let randomSphere () =
    Scene.sphere
        (Random.pointInBall () * 5.0f)
        (Random.uniform 0.3f 1.0f)
        (Material.surface (Random.uniform 0.4f 0.8f))

[
    List.init 30 (fun _ -> randomSphere ())
    [Scene.ambient 0.2f]
]
|> Scene.collect
|> Image.traceImage imageSize tracesPerPixel camera
|> Image.normalize
|> Image.gamma 2.2f
|> Image.dither (1.0f / 256.0f)
|> Image.saveBitmap "test"
|> shellOpen
|> ignore
