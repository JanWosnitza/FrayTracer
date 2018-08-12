namespace FrayTracer

open System.Numerics

type Lens =
    {
    NearPlaneSize : float32
    }

module Lens =
    let create (fieldOfView) =
        {
        NearPlaneSize = sin (fieldOfView * 0.5f)
        }

type ICamera =
    abstract UniformPixelToRay : x:float32 * y:float32 -> Ray

type Camera =
    {
    Position : Vector3
    Forward : Vector3
    UpScaled : Vector3
    RightScaled : Vector3
    }

    interface ICamera with
        member this.UniformPixelToRay(x, y) =
            {
            Position = this.Position
            Direction =
                this.Forward
                + x * this.RightScaled
                + y * this.UpScaled
                |> Vector3.normalize
            }


type LookAtCamera =
    {
    Position : Vector3
    LookAt : Vector3
    Up : Vector3
    Lens : Lens
    }

module Camera =
    let ofLookAt {Position=position; LookAt=lookAt; Up=up; Lens=lens} =
        let forward = (lookAt - position) |> Vector3.normalize
        let right = Vector3.cross forward up |> Vector3.normalize
        
        {
        Position = position
        Forward = forward
        UpScaled = Vector3.cross forward right * lens.NearPlaneSize
        RightScaled = right * lens.NearPlaneSize
        }

    let uniformPixelToRay (camera:ICamera) (struct (x, y)) = camera.UniformPixelToRay (x, y)
