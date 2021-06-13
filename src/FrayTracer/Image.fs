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
    let render (epsilon:float32) (length:float32) (imageSize:ImageSize) (camera) (trace:Ray -> FColor) =
        let getUniformPixelPos = ImageSize.getUniformPixelPos imageSize
        Array2D.Parallel.init imageSize.X imageSize.Y (fun x y ->
            let ray =
                Vector2(float32 x, float32 y)
                |> getUniformPixelPos
                |> Camera.uniformPixelToRay epsilon length camera
                    
            trace ray
        )

    let toColors (gamma:float32) (rng:System.Random) (image:FColor[,]) =
        let gammaInv = 1.0f / gamma

        let max =
            image
            |> Array2D.Parallel.maxWith FColor.getMaxColor
            |> MathF.max 0.01f

        image
        |> Array2D.Parallel.map (fun fcolor ->
            fcolor / max
            |> FColor.gammaInverse gammaInv
            |> FColor.toColor rng
        )

    [<Struct>]
    [<StructLayout(LayoutKind.Sequential)>]
    type BitmapColor =
        {
            mutable B : byte
            mutable G : byte
            mutable R : byte
        }

    let saveBitmap (path:string) (image:Color[,]) =
        let length1 = image.GetLength(0)
        let length2 = image.GetLength(1)

        let buffer =
            Array.Parallel.init (length1*length2) (fun index ->
                let x = index % length1
                let y = length2 - 1 - index / length1
                let color = image.[x,y]
                {B = color.B; G = color.G; R = color.R}
            )
            |> Array.rev
            //|> Seq.collect (fun color -> [|color.B; color.G; color.R|])
            //|> Seq.toArray
            //Array.rev

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
