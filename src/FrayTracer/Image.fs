namespace FrayTracer

open System.Runtime.InteropServices
open System.Drawing
open System.IO
open System.Drawing.Imaging

type ImageSize =
    {
    X : int
    Y : int
    }

module ImageSize =
    let getUniformPixelPos (size:ImageSize) (position:Vector2) =
        let maxSize = max size.X size.Y |> float32
        Vector2(
            position.X / maxSize,
            position.Y / maxSize
        )

module Image =
    let render (imageSize:ImageSize) (camera) (trace:Ray -> float32) =
        Array2D.Parallel.init imageSize.X imageSize.Y 
        <| fun x y ->
        Vector2(float32 x, float32 y)
        |> ImageSize.getUniformPixelPos imageSize
        |> Camera.uniformPixelToRay camera
        |> trace

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
        |> Array2D.map (fun x -> x + Random.uniform_11 () * half)

    let saveBitmap (path:string) (image:float32[,]) =
        let buffer =
            image
            |> Array2D.toSeq
            |> Seq.map (fun x -> x * 255.0f |> round |> uint8)
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
