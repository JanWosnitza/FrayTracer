open FrayTracer.Core
open System.Numerics
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

// Reflection
// Refraction
// Diffuse <> random reflection

let ray = 
    {
    Ray.Position = Vector3( -10.f, 0.1f, 0.f )
    Direction = Vector3( 1.f, 0.f, 0.f )
    Frequency = 10000.0<Hz>
    Time = 0.0<s>
    Coeff = 1.0f
    }

let ball (radius) (material) =
    let radius2 = radius * radius
    // center at (0, 0, 0)

    let test (ray:Ray) =
        let distanceNearest = -Vector3.Dot( ray.Position, ray.Direction )
        if distanceNearest <= 0.0001f then
            None
        else
            let nearest = ray.Position + ray.Direction * distanceNearest
            let length2 = nearest.LengthSquared()
            if length2 >= radius2 then
                None
            else
                let distance =
                    let s = sqrt (radius2 - length2)
                    let c = distanceNearest - s
                    if c >= 0.f then c
                    else distanceNearest + s

                {
                RayHit.Distance = distance
                Normal = Vector3.Normalize( ray.Position + ray.Direction * distance )
                Material = material
                } |> Some

    test

let glassBall = ball 1.f Material.glass

glassBall ray
|> printfn "%A"

let trace (volume:Volume) (ray) =
    let rec loop (ray) =
        printfn "Ray: %A" ray
        match volume ray with
        | Some hit ->
            printfn "Hit: %A" hit
            printfn ""
            Ray.reflect hit ray
            |> loop
        | None -> ray.Coeff * 10.0f

    loop ray

trace glassBall ray
|> printfn "%A"
