[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module FrayTracer.SdfObject

open System

let create (material) (form) =
    {
        Form = form
        Material = material
    }

let union (objects:seq<SdfObject>) =
    match objects |> Seq.toArray with
    | [||] -> failwith "No SdfObjects given."
    | [|object|] -> object
    | objects ->
        {
            Form =
                objects
                |> Seq.map (fun sdf -> sdf.Form)
                |> SdfForm.union

            Material =
                {
                    Color =
                        let getObjects = objects |> SdfBoundary.buildSpatialLookup (fun x -> x.Form.Boundary)
                        fun (position) ->
                        let objects = getObjects position
                        let mutable minSdf = objects.[0]
                        let mutable min = minSdf.Form.Distance position

                        for i = 1 to objects.Length - 1 do
                            let sdf = objects.[i]
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

let private testColor = FColor.ofRGB 1f 1f 1f

let tryTrace (object:SdfObject) (ray:Ray) : voption<SdfObjectTraceResult> =
    match  SdfForm.tryTrace object.Form ray with
    | ValueNone -> ValueNone
    | ValueSome resultRay ->
        let position = resultRay.Origin
        ValueSome {
            Ray = resultRay
            Normal = SdfForm.normal object.Form (ray.Epsilon * 0.1f) position
            Color = 
                //testColor
                object.Material |> SdfMaterial.getColor position
        }
