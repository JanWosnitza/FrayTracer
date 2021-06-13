[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module FrayTracer.SdfObject

open System

let create (material) (form) =
    {
        Form = form
        Material = material
    }

let union (sdfs:seq<SdfObject>) =
    match sdfs |> Seq.toArray with
    | [||] -> failwith "No SdfObjects given."
    | [|sdf|] -> sdf
    | sdfs ->
        {
            Form =
                sdfs
                |> Seq.map (fun sdf -> sdf.Form)
                |> SdfForm.union

            Material =
                {
                    Color =
                        fun (position) ->
                        let mutable minSdf = sdfs.[0]
                        let mutable min = minSdf.Form.Distance position

                        for i = 1 to sdfs.Length - 1 do
                            let sdf = sdfs.[i]
                            if min > SdfBoundary.getMinDistance sdf.Form.Boundary position then
                                let distance = sdf.Form.Distance position
                                if distance < min then
                                    min <- distance
                                    minSdf <- sdf
                
                        minSdf.Material
                        |> SdfMaterial.getColor position
                }
        }

let subtract (object:SdfObject) (form:SdfForm) =
    {
        Form = SdfForm.subtract object.Form form
        Material = object.Material
    }

let intersect (object:SdfObject) (forms:seq<SdfForm>) =
    {
        Form =
            seq {
                yield object.Form
                yield! forms
            } |> SdfForm.intersect
        Material = object.Material
    }

let tryTrace (object:SdfObject) (ray:Ray) : voption<SdfObjectTraceResult> =
    match  SdfForm.tryTrace object.Form ray with
    | ValueNone -> ValueNone
    | ValueSome resultRay ->
        let position = resultRay.Origin
        ValueSome {
            Ray = resultRay
            Normal = SdfForm.normal object.Form (ray.Epsilon * 0.1f) position
            Color = object.Material |> SdfMaterial.getColor position
        }

let cached (width) (object:SdfObject) =
    {object with
        Form = SdfForm.cached width object.Form
    }
