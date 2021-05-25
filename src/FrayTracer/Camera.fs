namespace FrayTracer

type Lens =
    {
    NearPlaneSize : float32
    }

module Lens =
    let create (fieldOfView) =
        {
        NearPlaneSize = sin (fieldOfView * 0.5f)
        }

type Camera =
    {
    Position : Vector3
    Forward : Vector3
    UpScaled : Vector3
    RightScaled : Vector3
    }

module Camera =
    type LookAt =
        {
            Position : Vector3
            LookAt : Vector3
            Up : Vector3
            Lens : Lens
        }

    let lookAt (camera:LookAt) =
        let forward = (camera.LookAt - camera.Position) |> Vector3.normalized
        let right = Vector3.cross camera.Up forward |> Vector3.normalized
        
        {
        Position = camera.Position
        Forward = forward
        UpScaled = Vector3.cross forward right * camera.Lens.NearPlaneSize
        RightScaled = right * camera.Lens.NearPlaneSize
        }

    let uniformPixelToRay (epsilon:float32) (camera:Camera) (position:Vector2) =
        {
        Epsilon = epsilon
        Origin = camera.Position
        Direction =
            camera.Forward
            + (position.X - 0.5f) * camera.RightScaled
            + (position.Y - 0.5f) * camera.UpScaled
            |> Vector3.normalized
        }
