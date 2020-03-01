namespace FrayTracer.SDF

open System
open FrayTracer

type ISignedDistanceField =
    abstract GetDistance : position:Vector3 -> float32

module Primitive =
    [<Struct>]
    type Sphere =
        {
            Center : Vector3
            Radius : float32
        }

        interface ISignedDistanceField with
            member this.GetDistance(position) =
                Vector3.Distance(this.Center, position) - this.Radius

    let sphere (x:Sphere) = x  :> ISignedDistanceField

module Combine =
    [<Struct>]
    type Union<'a, 'b
        when 'a :> ISignedDistanceField
        and 'b :> ISignedDistanceField
        > =
        {
            mutable A:'a
            mutable B:'b
        }

        interface ISignedDistanceField with
            member this.GetDistance(position) =
                Math.Min(
                    this.A.GetDistance(position),
                    this.B.GetDistance(position)
                )

    let union (a:ISignedDistanceField) (b:ISignedDistanceField) =
        {
            Union.A = a
            B = b
        } :> ISignedDistanceField

    [<Struct>]
    type UnionSmooth<'a, 'b
        when 'a :> ISignedDistanceField
        and 'b :> ISignedDistanceField
        > =
        {
            Strength : float32
            mutable A:'a
            mutable B:'b
        }

        interface ISignedDistanceField with
            member this.GetDistance(position) =
                let a = this.A.GetDistance(position)
                let b = this.B.GetDistance(position)
                let res = Math.Exp(float (-this.Strength * a)) + Math.Exp(float (-this.Strength * b))
                float32 <| (-Math.Log( res ) / float this.Strength)

    let unionSmooth (strength:float32) (a:ISignedDistanceField) (b:ISignedDistanceField) =
        {
            UnionSmooth.Strength = strength
            A = a
            B = b
        } :> ISignedDistanceField

    [<Struct>]
    type Intersection<'a, 'b
        when 'a :> ISignedDistanceField
        and 'b :> ISignedDistanceField
        > =
        {
            mutable A:'a
            mutable B:'b
        }
        interface ISignedDistanceField with
            member this.GetDistance(position) =
                Math.Max(
                    this.A.GetDistance(position),
                    this.A.GetDistance(position)
                )

    let intersection (a:ISignedDistanceField) (b:ISignedDistanceField) =
        {
            Intersection.A = a
            B = b
        } :> ISignedDistanceField

    [<Struct>]
    type Subtraction<'a, 'b
        when 'a :> ISignedDistanceField
        and 'b :> ISignedDistanceField
        > =
        {
            mutable A:'a
            mutable B:'b
        }
        interface ISignedDistanceField with
            member this.GetDistance(position) =
                Math.Max(
                    this.A.GetDistance(position),
                    -this.B.GetDistance(position)
                )

    let subtraction (a:ISignedDistanceField) (b:ISignedDistanceField) =
        {
            Intersection.A = a
            B = b
        } :> ISignedDistanceField

    module Operators =
        let inline (<|>) (a) (b) = union a b
        let inline (<&>) (a) (b) = intersection a b
        let inline (<->) (a) (b) = intersection a b

module Test =
    // convert the whole nested tree into a single (generic) struct
    let compile (sdf:ISignedDistanceField) =
        let bindingFlags =
            Reflection.BindingFlags.Instance
            ||| Reflection.BindingFlags.Public
            ||| Reflection.BindingFlags.NonPublic

        let rec loop (sdf:ISignedDistanceField) =
            let t = sdf.GetType()
            if  t.IsValueType
                && t.IsConstructedGenericType
            then
                let tGeneric = t.GetGenericTypeDefinition()
                let genericArguments = t.GetGenericArguments()

                let fields = t.GetFields(bindingFlags)

                let values =
                    fields
                    |> Array.zip (tGeneric.GetFields(bindingFlags) |> Array.map (fun x -> x.FieldType))
                    |> Array.map (fun (genericType, field) ->
                        let value = field.GetValue(sdf)

                        if  genericType.IsGenericParameter
                            && typeof<ISignedDistanceField> = field.FieldType
                        then
                            let valueCompiled = loop (value :?> ISignedDistanceField) |> box
                            genericArguments.[genericType.GenericParameterPosition] <- valueCompiled.GetType()
                            valueCompiled
                        else
                            value
                        )

                let instance = Activator.CreateInstance(t)

                (fields, values)
                ||> Seq.iter2 (fun (field) (value) -> field.SetValue(instance, value))

                instance :?> ISignedDistanceField
            else
                sdf

        loop sdf

    let trace (epsilon:float32) (sdf:ISignedDistanceField) (ray:Ray) =
        let direction = ray.Direction
        let position = ray.Position

        let rec test (position:Vector3) (length:float32) =
            let distance = sdf.GetDistance(position)
            if distance < epsilon then
                ValueSome position
            elif length > 10000.0f then
                ValueNone
            else
                test (position + direction * distance) (length + distance)

        let distance = sdf.GetDistance(position)
        test (position + direction * distance) distance

    let normal (epsilon:float32) (sdf:ISignedDistanceField) (position:Vector3) =
        let inline f (dimension:Vector3) =
            let epsilon = dimension * epsilon
            sdf.GetDistance(position - epsilon) - sdf.GetDistance(position + epsilon)

        Vector3(f Vector3.UnitX, f Vector3.UnitY, f Vector3.UnitZ)
        |> Vector3.normalized

    let traceWithDirectionalLigth (epsilon:float32) (lightDirection:Vector3) (sdf:ISignedDistanceField) =
        let inline trace (ray) = trace epsilon sdf ray
        let normalEpsilon = epsilon / 10.0f
        let inline normal (position) = normal normalEpsilon sdf position

        fun (ray:Ray) ->
        match  trace ray with
        | ValueSome position ->
            let light =
                (*// shadow
                let ray = {
                    Position = position - lightDirection * epsilon
                    Direction = -lightDirection
                }
                match trace ray with
                | ValueNone ->
                    lightDirection
                    |> Vector3.dot (normal position)
                    |> Math.max 0.0f
                | ValueSome _ ->
                    0.0f*)

                Vector3.Dot(normal position, lightDirection)
                |> Math.max 0.0f

            0.1f + light * 0.9f
        | ValueNone -> 0.0f
