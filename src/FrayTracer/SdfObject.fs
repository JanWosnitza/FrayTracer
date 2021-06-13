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
                        let mutable material = objects.Items.[0].Item.Material
                        let mutable min = objects.Items.[0].Item.Form.Distance position

                        let distanceToCenter = position |> Vector3.distance objects.Center

                        for i = 0 to objects.Items.Length - 1 do
                            let mutable sdf = &objects.Items.[i]
                            if
                                min > sdf.LowerBound - distanceToCenter
                                && min > SdfBoundary.getMinDistance sdf.Item.Form.Boundary position
                            then
                                let distance = sdf.Item.Form.Distance position
                                if distance < min then
                                    min <- distance
                                    material <- sdf.Item.Material

                        position
                        |> SdfMaterial.getColor material
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
    | ValueSome result ->
        let position = result.Ray.Origin
        let normal = SdfForm.normal object.Form (ray.Epsilon * 0.125f) (position, result.Distance)
        ValueSome {
            Ray = result.Ray
            Normal = normal
            Color = 
                position //- normal * ray.Epsilon
                |> SdfMaterial.getColor object.Material
        }
