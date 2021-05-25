namespace FrayTracer

open System.Numerics
open System

type Undefined = unit

[<Struct>]
type Ray =
    {
        Epsilon : float32
        Origin : Vector3
        /// <summary>Normalized direction of the ray.</summary>
        Direction : Vector3
    }


module Ray =
    let get (length:float32) (ray:Ray) = ray.Origin + ray.Direction * length
    let move (length:float32) (ray:Ray) = {ray with Origin = ray |> get length}
    let setDirection (direction:Vector3) (ray:Ray) =
        {ray with Direction = direction}

type Material =
    {
    IntensityCoeff : float32
    RefractionIndex : float32
    Intensity : float32
    }
