namespace FrayTracer.Core

open Space

type Ray = {
    Position : Position
    Direction : Direction
    }

type RayHit = {
    Distance : float<m>
    Normal : Lazy<Direction>
    }

type Bounding =
    | Sphere of Position * float<m>
    //| Box of ???

type Volume = {
    Boudning : Bounding
    IsInside : Position -> bool
    Intersect : Ray -> RayHit option
    }

type HittedRay = {
    Ray : Ray
    Normal : Direction
    } with
    static member OfHit( ray:Ray, hit:RayHit ) =
        {
        Ray = {ray with Position = ray.Position + ray.Direction * hit.Distance}
        Normal = hit.Normal.Value
        }

// Is responsible for reflectance, transmitance and marshaling
// Thus: Material is a property of Surface, so Surface might be a bad name
type Surface = {
    Hit : HittedRay -> Ray
    }

