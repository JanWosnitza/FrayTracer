[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module FrayTracer.Scene

open System
open System.Numerics

let empty : Scene = fun (ray) -> None

let combine (s1:Scene) (s2:Scene) : Scene =
    fun (ray) ->
    match s1 ray, s2 ray with
    | Some r1, Some r2 -> if r1.Length < r2. Length then Some r1 else Some r2
    | Some r1, None -> Some r1
    | None, Some r2 -> Some r2
    | None, None -> None

let collect (xs:#seq<Scene>) =
    xs
    |> Seq.fold combine empty

let ambient (intensity) : Scene =
    let material = Material.light intensity

    fun ray ->
    Some {
    Length = Single.PositiveInfinity
    Normal = -ray.Direction
    Material = material
    }

let sphere (position:Vector3) (radius:float32) (material) : Scene =
    fun (ray) ->
    let p = (position - ray.Position) / radius
    let lenNearest = Vector3.Dot(p, ray.Direction)
    let pNearest = p - ray.Direction * lenNearest
    let lenNearestSquared = pNearest.LengthSquared()
    if lenNearestSquared > 1.0f then
        None
    else
        let lenBoundary = sqrt (1.0f - lenNearestSquared)
        if lenNearest < lenBoundary then
            None
        else
            let pBoundary = pNearest + ray.Direction * lenBoundary
            Some {
            Length = (lenNearest - lenBoundary) * radius
            Normal = Vector3.normalize pBoundary
            Material = material
            }

let trace (scene:Scene) (ray:Ray) : float32 =
    let rec loop (intensity) (intensityCoeff) (ray) =
        if intensityCoeff < 0.01f then
            intensity
        else
            match scene ray with
            | None -> intensity
            | Some result ->
                let pHit = ray.Position + ray.Direction * result.Length
                let direction = Vector3.Reflect(ray.Direction, result.Normal)
                let ray =
                    {
                    Position = pHit + direction * 0.01f
                    Direction = direction
                    }

                loop (intensity + result.Material.Intensity * intensityCoeff)
                     (intensityCoeff * result.Material.IntensityCoeff)
                     ray

    loop 0.0f 1.0f ray