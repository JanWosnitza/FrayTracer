open System
open FrayTracer.Core
open System.Numerics
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

// Reflection
// Refraction
// Diffuse = dot( normal, direction )

let ray = 
    Ray.start
        10000.0<Hz>
        (Vector3( -10.f, 0.1f, 0.f ))
        (Vector3( 1.f, 0.f, 0.f ))

let background (glow) =
    let material = {
        Material.Name = "Background"
        Color = (fun _ -> 0.f)
        RefractionIndex = 1.f
        Glow = glow
    }

    let test (ray:Ray) =
        {
        RayHit.Distance = Single.MaxValue * 1.f<_>
        Material = material
        Normal = Vector3.UnitX
        } |> Some
    
    test

let ball (position) (radius) (material) =
    let radius2 = radius * radius
    // center at (0, 0, 0)

    let test (ray:Ray) =
        let pos = ray.Position - position
        let dir = ray.Direction
        let distanceNearest = -Vector3.Dot( pos, dir )
        if distanceNearest <= 0.0001f then
            None
        else
            let nearest = pos + dir * distanceNearest
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
                RayHit.Distance = distance * 1.f<_>
                Normal = Vector3.Normalize( pos + dir * distance )
                Material = material
                } |> Some

    test

let glassBall = ball Vector3.Zero 1.f Material.glass

//glassBall ray
//|> printfn "%A"

let (++) (v1:Volume) (v2:Volume) (ray) =
    let r1 = v1 ray
    let r2 = v2 ray
    match (r1, r2) with
    | (None, None) -> None
    | (Some _, None) -> r1
    | (None, Some _) -> r2
    | (Some r1', Some r2') ->
        if r1'.Distance < r2'.Distance then
            r1
        else r2

let trace (volume:Volume) (ray) =
    let rec loop (ray) =
        printfn "##### Step #####\n%A" ray
        if ray.Coeff < 0.001f then
            ray.Intensity
        else
            match volume ray with
            | Some hit ->
                printfn "\n%A" hit
                printfn ""
                let fres = Light.fresnel (1.f) (hit.Material.RefractionIndex) (hit.Normal) (ray.Direction)
                {ray with
                    Position = ray.Position + ray.Direction * float32 hit.Distance
                    Direction = fres.Reflect
                    Coeff = ray.Coeff * fres.Reflectance
                }
                |> loop
            | None -> ray.Intensity

    loop ray

let myBackground = background (fun _ -> 10.f)

trace (glassBall ++ myBackground) ray
|> printfn "%A"
