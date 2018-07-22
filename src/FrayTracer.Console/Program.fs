module Program

open System
open System.Numerics
open FrayTracer.Core
open System.Reflection
open System.Runtime.InteropServices
open System.Drawing
open System.Drawing.Imaging

let (++) (s1) (s2) = Scene.combine s1 s2

type Camera =
    {
    Position : Vector3
    Direction : Vector3
    Up : Vector3
    Fov : float32
    Near : float32
    }

type ImageSize =
    {
    SizeX : int
    SizeY : int
    }

let getUniformPixelPos (size:ImageSize) (x, y) =
    let ux = (float32 x + Random.uniform_01 ()) / float32 size.SizeX - 0.5f
    let uy = (float32 y + Random.uniform_01 ()) / float32 size.SizeY - 0.5f

    (ux, uy)

let uniformPixelToRay (camera:Camera) (x, y) : Ray =
    {
    Position = vector3 0.0f 0.0f 0.0f
    Direction = vector3 0.0f 0.0f 0.0f
    }

let toRays (scene) (camera) (imageSize) (x, y) =
    let trace () = 
        (x, y)
        |> getUniformPixelPos imageSize
        |> uniformPixelToRay camera
        |> Scene.trace scene
        
    Seq.initInfinite ignore
    |> Seq.map trace

let tracesPerPixel = 10

module Array2D =
    let toSeqIndexed (array:_[,]) =
        seq {
        for i = 0 to array.GetLength(0) do
            for j = 0 to array.GetLength(1) do
                yield (i, j), array.[i, j]
        }

    let toSeq (array:_[,]) =
        seq {
        for i = 0 to array.GetLength(0) do
            for j = 0 to array.GetLength(1) do
                yield array.[i, j]
        }

    let max (array:_[,]) =
        array
        |> toSeq
        |> Seq.max

module Image =
    let traceImage (imageSize:ImageSize) (pixelToIntensities) =
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
        image
        |> Array2D.map (fun x -> x ** gamma)

    let dither (max:float32) (image:float32[,]) =
        let half = max * 0.5f
        image
        |> Array2D.map (fun x -> x + Random.uniform -half half)

    let saveBitmap (path:string) (image:float32[,]) =
        use bm =
            let scan0 = GCHandle.Alloc(image, GCHandleType.Pinned)
            try
                new Bitmap(
                    image.GetLength(0),
                    image.GetLength(1),
                    image.GetLength(1)*3,
                    PixelFormat.Format24bppRgb,
                    scan0.AddrOfPinnedObject()
                )
            finally scan0.Free()
        bm.Save(path + ".bmp", ImageFormat.Bmp)
        

////////////////////////////////
// MAIN
 
let scene =
    Scene.ambient 0.2f
    ++ Scene.sphere
        (vector3 0.0f 0.0f 0.0f, 1.0f)
        (Material.surface 0.5f)
    ++ Scene.sphere
        (vector3 2.0f 0.0f 0.0f, 1.0f)
        (Material.light 2.0f)
 
let camera =
    {
    Position = vector3 0.0f -2.0f 0.0f
    Direction = vector3 0.0f 1.0f 0.0f
    Up = vector3 0.0f 0.0f 1.0f
    Fov = 90.0f * Math.degToRad
    Near = 0.1f
    }

let imageSize =
    {
    SizeX = 500
    SizeY = 500
    }

let image =
    Image.traceImage imageSize (toRays scene camera)
    |> Image.normalize
    |> Image.gamma 2.2f
    |> Image.saveBitmap "test"
