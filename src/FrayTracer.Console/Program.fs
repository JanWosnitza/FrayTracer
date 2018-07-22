module Program

open System
open System.Numerics
open FrayTracer.Core
open System.Reflection
open System.Runtime.InteropServices
open System.Drawing
open System.Drawing.Imaging
open System.IO
open System.Diagnostics

let (++) (s1) (s2) = Scene.combine s1 s2

type Camera =
    {
    Position : Vector3
    Forward : Vector3
    Up : Vector3
    Right : Vector3
    NearPlaneDistance : float32
    NearPlaneSize : float32
    }

type ImageSize =
    {
    SizeX : int
    SizeY : int
    }

let getUniformPixelPos (size:ImageSize) (x, y) =
    let uniform (x) = float32 x + Random.uniform_01 () - 0.5f
    let maxSize = max size.SizeX size.SizeY |> float32
    let ux = (uniform x - float32 size.SizeX / 2.0f) / maxSize
    let uy = (uniform y - float32 size.SizeY / 2.0f) / maxSize

    (ux, uy)

let uniformPixelToRay (camera:Camera) (x, y) : Ray =
    let v : Vector3 = (x * camera.Right + y * camera.Up)
    let dir = camera.Forward * camera.NearPlaneDistance + v * camera.NearPlaneSize
    {
    Position = camera.Position + dir
    Direction = dir |> Vector3.normalize
    }

let toRays (scene) (camera) (imageSize) (x, y) =
    let trace () = 
        (x, y)
        |> getUniformPixelPos imageSize
        |> uniformPixelToRay camera
        |> Scene.trace scene
        
    Seq.initInfinite ignore
    |> Seq.map trace

module Array2D =
    let toSeqIndexed (array:_[,]) =
        seq {
        for i = 0 to array.GetLength(0) - 1 do
            for j = 0 to array.GetLength(1) - 1 do
                yield (i, j), array.[i, j]
        }

    let toSeq (array:_[,]) =
        seq {
        for i = 0 to array.GetLength(0) - 1 do
            for j = 0 to array.GetLength(1) - 1 do
                yield array.[i, j]
        }

    let max (array:_[,]) =
        array
        |> toSeq
        |> Seq.max

module Image =
    let traceImage (imageSize:ImageSize) (tracesPerPixel) (pixelToIntensities) =
        Array2D.init imageSize.SizeX imageSize.SizeX
            (fun x y ->
            pixelToIntensities imageSize (x, y) :> float32 seq
            |> Seq.take tracesPerPixel
            |> Seq.average
            )            

    let normalize (image:float32[,]) =
        let max = Array2D.max image
        image
        |> Array2D.map (fun x -> x / max)
    
    let gamma (gamma:float32) (image:float32[,]) =
        let power = 1.0f / gamma
        image
        |> Array2D.map (fun x -> x ** power)

    let dither (max:float32) (image:float32[,]) =
        let half = max * 0.5f
        image
        |> Array2D.map (fun x -> x + Random.uniform -half half)

    let saveBitmap (path:string) (image:float32[,]) =
        let buffer =
            image
            |> Array2D.toSeq
            |> Seq.map (fun x -> uint8 (x * 255.0f))
            |> Seq.collect (Seq.replicate 3)
            |> Seq.toArray

        use bm =
            let scan0 = GCHandle.Alloc(buffer, GCHandleType.Pinned)
            try
                new Bitmap(
                    image.GetLength(0),
                    image.GetLength(1),
                    image.GetLength(0)*3,
                    PixelFormat.Format24bppRgb,
                    scan0.AddrOfPinnedObject()
                )
            finally scan0.Free()
        let fullPath = Path.GetFullPath(path + ".bmp")
        bm.Save(fullPath, ImageFormat.Bmp)
        fullPath
        
let shellOpen (path) =
    Process.Start(ProcessStartInfo(path, UseShellExecute = true ))

////////////////////////////////
// MAIN
 
let camera =
    {
    Position = vector3 0.0f -4.0f 0.0f
    Forward = vector3 0.0f 1.0f 0.0f
    Up = vector3 0.0f 0.0f 1.0f
    Right = vector3 1.0f 0.0f 0.0f
    NearPlaneDistance = 0.1f
    NearPlaneSize = sin (60.0f * Math.degToRad * 0.5f)
    }

let tracesPerPixel = 4

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

let scene =
    [
        List.init 30 (fun _ -> randomSphere ())
        [Scene.ambient 0.2f]
    ]
    |> Scene.collect

Image.traceImage imageSize tracesPerPixel (toRays scene camera)
|> Image.normalize
|> Image.gamma 2.2f
|> Image.dither (1.0f / 256.0f)
|> Image.saveBitmap "test"
|> shellOpen
|> ignore
