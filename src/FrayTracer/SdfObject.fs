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

let subtraction (object:SdfObject) (forms:seq<SdfForm>) =
    {
        Form = SdfForm.subtraction object.Form forms
        Material = object.Material
    }

let intersection (object:SdfObject) (forms:seq<SdfForm>) =
    {
        Form =
            seq {
                yield object.Form
                yield! forms
            } |> SdfForm.intersection
        Material = object.Material
    }

let tryTrace (object:SdfObject) (length:float32) (ray:Ray) : voption<SdfObjectTraceResult> =
    match  SdfForm.tryTrace object.Form length ray with
    | ValueNone -> ValueNone
    | ValueSome result ->
        ValueSome {
            Position = result.Position
            Normal = SdfForm.normalFast object.Form (ray.Epsilon / 500f) result.Position result.Distance
            Color = object.Material |> SdfMaterial.getColor result.Position
        }
