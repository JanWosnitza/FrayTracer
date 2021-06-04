namespace FrayTracer

open System.Numerics
open System.Runtime.InteropServices
open System.Drawing
open System.IO
open System.Drawing.Imaging

[<Struct>]
type ImageSize =
    {
    X : int
    Y : int
    }

module ImageSize =
    let getUniformPixelPos (size:ImageSize) =
        let maxSize = max size.X size.Y |> float32
        fun (position:Vector2) ->
        Vector2(
            position.X / maxSize,
            position.Y / maxSize
        )

module Image =
    let render (imageSize:ImageSize) (camera) (epsilon:float32) (trace:Ray -> FColor) =
        let getUniformPixelPos = ImageSize.getUniformPixelPos imageSize
        Array2D.Parallel.init imageSize.X imageSize.Y (fun x y ->
            let ray =
                Vector2(float32 x, float32 y)
                |> getUniformPixelPos
                |> Camera.uniformPixelToRay epsilon camera
                    
            trace ray
        )

    let normalize (image:FColor[,]) =
        let max =
            image
            |> Array2D.toSeq
            |> Seq.map (FColor.getMaxColor)
            |> Seq.max

        image
        |> Array2D.map (fun x -> x / max)

    let gamma (gamma:float32) (image:FColor[,]) =
        let gammaInv = 1.0f / gamma
        image
        |> Array2D.map (FColor.gammaInverse gammaInv)

    let toColors (rng:System.Random) (image:FColor[,]) =
        image
        |> Array2D.map (FColor.toColor rng)

    let saveBitmap (path:string) (image:Color[,]) =
        let buffer =
            image
            |> Array2D.toSeq
            |> Seq.collect (fun color -> [|color.B; color.G; color.R|])
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
