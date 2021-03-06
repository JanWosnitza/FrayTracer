﻿// Modified version of: http://plinth.org/techtalk/?p=51

open System
open System.Drawing
open System.Drawing.Imaging
open System.Runtime.InteropServices
open System.Diagnostics

[<Measure>] type intensity
[<Measure>] type colorspace

type Intensity = float32<intensity>
type ColorSpace = float32<colorspace>

type Vector = {
    X:single; Y:single; Z:single
}

let inline dot u v = u.X * v.X + u.Y * v.Y + u.Z * v.Z

let length2 v = dot v v

let inline length v = length2 v |> sqrt

let normalize v =
    let len = length v
    { X = v.X / len; Y = v.Y / len; Z = v.Z / len }

let cosanglebetween u v = (dot u v) / (length u * length v)

let pointOnSphere r x y =
    // x^2 + y^2 + z^2 = r^2
    // z^2 = r^2 - x^2 - y^2
    let z2 = (r * r) - (x * x) - (y * y)
    if z2 >= 0.0f then
        let z = sqrt z2
        Some(z, -z)
    else None

let illum color (ambient:Intensity) lightpos (lightcolor:Intensity) r x y =
    let lighting z1 z2 =
        let v1 = { X = x; Y = y; Z = z1 }
        let v2 = { X = x; Y = y; Z = z2 }
        let v = if length2 v1 < length2 v2 then v1 else v2
        let cosangle = cosanglebetween lightpos v
        let lit = if cosangle < 0.0f then 0.0f else cosangle
        (lit * lightcolor + ambient) * color
    match pointOnSphere r x y with
    | Some(posz, negz) -> lighting posz negz
    | None -> ambient

let kSphereRad = 512.0f
let kLightPos = { X = 2048.0f; Y = -2048.0f; Z = -2048.0f }
let kAmbient = 0.2f<intensity>
let kLightColor = 1.f<intensity>
let kColor = 1.0f<intensity> / (kLightColor + kAmbient)

let myIllum (x) (y) =
    illum kColor kAmbient kLightPos kLightColor kSphereRad
          (float32 x - kSphereRad) (float32 y - kSphereRad)

let toColorSpace (brightness:Intensity) : ColorSpace = brightness * 256.f<_>

let rnd = Random()

let ditherPosition (samples) (f:_->_->Intensity) (x) (y) =
    let mutable sum = 0.0f<intensity>
    for _ in 1 .. samples do
        let v = f (x + float32 (rnd.NextDouble())) 
                  (y + float32 (rnd.NextDouble()))
        sum <- sum + v
    sum / float32 samples

let ditherRandom (brightness:ColorSpace) =
    let diff = rnd.NextDouble() - 0.5 |> float32
    brightness + diff * 1.f<colorspace>

let quantize brightness =
    brightness * 1.f<1/colorspace> + 0.5f
    |> max 0.f |> min 255.f |> byte

let saveJpeg (buffer:byte[], width, height) (filename:string, format) =
    use bm =
        let scan0 = GCHandle.Alloc( buffer, GCHandleType.Pinned )
        try new Bitmap( width, height, height*3, PixelFormat.Format24bppRgb, scan0.AddrOfPinnedObject() )
        finally scan0.Free()
    bm.Save(filename, format)

let toByte = ditherRandom >> quantize


[<EntryPoint>]
let main argv =
    let stopwatch = Stopwatch.StartNew()
    let updateInterval = TimeSpan.FromSeconds( 0.1 )
    let mutable nextUpdate = updateInterval
    
    let l = int (2.0f * kSphereRad)
    let buffer = Array.zeroCreate (l*l*9)
    let mutable i = 0
    for y = 0 to l - 1 do
        for x = 0 to l - 1 do
            let pv =
                ditherPosition 100 myIllum (float32 x) (float32 y)
                |> toColorSpace
            buffer.[ i + 0 ] <- toByte pv
            buffer.[ i + 1 ] <- toByte pv
            buffer.[ i + 2 ] <- toByte pv
            i <- i + 3

            let elapsed = stopwatch.Elapsed
            if elapsed >= nextUpdate then
                let percentage = float (y * l + x) / float (l*l)
                let remainingTime = elapsed.TotalSeconds * ( 1.0 / percentage - 1.0) |> TimeSpan.FromSeconds
                printf "\r%5.1f%% - %O" (percentage * 100.0) remainingTime
                nextUpdate <- nextUpdate + updateInterval

    printf "\r"

    stopwatch.Stop()
    printfn "total time: %O\n" stopwatch.Elapsed

    let file = "output.bmp"
    saveJpeg (buffer, l, l) (file, ImageFormat.Bmp)
    Process.Start( file ) |> ignore
    0
