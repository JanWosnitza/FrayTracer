namespace FrayTracer

open System.Runtime.InteropServices
open System.Drawing
open System.IO
open System.Drawing.Imaging

type ImageSize =
    {
    SizeX : int
    SizeY : int
    }

module ImageSize =
    let getUniformPixelPos (size:ImageSize) (x, y) =
        let uniform (x) = float32 x + Random.uniform_01 () - 0.5f
        let maxSize = max size.SizeX size.SizeY |> float32
        let ux = (uniform x - float32 size.SizeX / 2.0f) / maxSize
        let uy = (uniform y - float32 size.SizeY / 2.0f) / maxSize

        struct (ux, uy)


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
