﻿namespace FrayTracer.SDF

open System
open FrayTracer

type ISignedDistanceField =
    abstract GetDistance : position:Vector3 -> float32

module Helper =
    let private bindingFlags =
        Reflection.BindingFlags.Instance
        ||| Reflection.BindingFlags.Public
        ||| Reflection.BindingFlags.NonPublic

    let construct<'Type when 'Type : struct> (parameters:obj[]) =
        let t = typedefof<'Type>

        let getCtor (t:Type) =
            let parameterCount = parameters.Length
            t.GetConstructors(bindingFlags)
            |> Seq.find (fun x -> x.GetParameters().Length = parameterCount)

        let ctor = getCtor t

        let genericParameters = t.GetGenericArguments()
        (ctor.GetParameters(), parameters)
        ||> Seq.iter2 (fun param value ->
            if param.ParameterType.IsGenericParameter then
                genericParameters.[param.ParameterType.GenericParameterPosition] <- value.GetType()
            )

        let ctor = getCtor (t.MakeGenericType(genericParameters))
        ctor.Invoke(parameters)

    let construct'<'Type when 'Type : struct> (parameters:obj[]) =
        let ctor = typeof<'Type>.GetConstructors(bindingFlags).[0]
        ctor.Invoke(parameters)

module Primitive =
    [<Struct>]
    type Sphere =
        {
            Center : Vector3
            Radius : float32
        }

        interface ISignedDistanceField with
            member this.GetDistance(position) =
                //let x = position.X - this.Center.X
                //let y = position.Y - this.Center.Y
                //let z = position.Z - this.Center.Z
                //MathF.Sqrt(x * x + y * y + z * z) - this.Radius
                Vector3.Distance(this.Center, position) - this.Radius

    let sphere (x:Sphere) = x  :> ISignedDistanceField

module Combine =
    [<Struct>]
    type private Union<'a, 'b
        when 'a :> ISignedDistanceField
        and 'b :> ISignedDistanceField
        > =
        {
            mutable A:'a
            mutable B:'b
        }

        interface ISignedDistanceField with
            member this.GetDistance(position) =
                MathF.Min(
                    this.A.GetDistance(position),
                    this.B.GetDistance(position)
                )

    let union (a:ISignedDistanceField) (b:ISignedDistanceField) =
        Helper.construct<Union<_, _>> [|
            box a
            box b
        |] :?> ISignedDistanceField

    [<Struct>]
    type private Intersection<'a, 'b
        when 'a :> ISignedDistanceField
        and 'b :> ISignedDistanceField
        > =
        {
            mutable A:'a
            mutable B:'b
        }
        interface ISignedDistanceField with
            member this.GetDistance(position) =
                MathF.Max(
                    this.A.GetDistance(position),
                    this.B.GetDistance(position)
                )

    let intersection (a:ISignedDistanceField) (b:ISignedDistanceField) =
        Helper.construct<Intersection<_, _>> [|
            box a
            box b
        |] :?> ISignedDistanceField

    [<Struct>]
    type private Subtraction<'a, 'b
        when 'a :> ISignedDistanceField
        and 'b :> ISignedDistanceField
        > =
        {
            mutable A:'a
            mutable B:'b
        }
        interface ISignedDistanceField with
            member this.GetDistance(position) =
                MathF.Max(
                    this.A.GetDistance(position),
                    -this.B.GetDistance(position)
                )

    let subtraction (a:ISignedDistanceField) (b:ISignedDistanceField) =
        Helper.construct<Subtraction<_, _>> [|
            box a
            box b
        |] :?> ISignedDistanceField

    type private IUnionSmoothSum =
        abstract Get : strength:float32 * position:Vector3 -> float32

    [<Struct>]
    type private UnionSmoothSumSingle<'sdf when 'sdf :> ISignedDistanceField> =
        {
        mutable Sdf : 'sdf
        }

        interface IUnionSmoothSum with
            member this.Get(strength, position) =
                MathF.Exp(-strength * this.Sdf.GetDistance(position))

    [<Struct>]
    type private UnionSmoothSumCombine<'sum, 'sdf
        when 'sum :> IUnionSmoothSum
        and 'sdf :> ISignedDistanceField> =
        {
        mutable Sum : 'sum
        mutable Sdf : 'sdf
        }

        interface IUnionSmoothSum with
            member this.Get(strength, position) =
                this.Sum.Get(strength, position) + MathF.Exp(-strength * this.Sdf.GetDistance(position))

    [<Struct>]
    type private UnionSmooth<'sum
        when 'sum :> IUnionSmoothSum
        > =
        {
            Strength : float32
            mutable Sum : 'sum
        }

        interface ISignedDistanceField with
            member this.GetDistance(position) =
                float32 <| (-MathF.Log(this.Sum.Get(this.Strength, position)) / this.Strength)

    let unionSmooth (strength:float32) (sdfs:ISignedDistanceField list) =
        match sdfs with
        | [] -> failwithf "blub"
        | [sdf] -> sdf
        | sdf :: sdfs ->
            let sum =
                (Helper.construct<UnionSmoothSumSingle<_>> [|box sdf|] :?> IUnionSmoothSum, sdfs)
                ||> List.fold (fun sum sdf -> Helper.construct<UnionSmoothSumCombine<_, _>> [|box sum; box sdf|] :?> IUnionSmoothSum)

            Helper.construct<UnionSmooth<_>> [|
                box strength
                box sum
            |] :?> ISignedDistanceField

    [<Struct>]
    type private Cache<'sdf
        when 'sdf :> ISignedDistanceField
        > =
        {
            Width : float32
            HalfDiagonal : float32
            Epsilon : float32
            CachedDistances : System.Collections.Concurrent.ConcurrentDictionary<struct (int * int * int), float32>
            mutable Sdf : 'sdf
        }

        interface ISignedDistanceField with
            member this.GetDistance(position) =
                let width = this.Width
                let x = MathF.roundToInt (position.X / width)
                let y = MathF.roundToInt (position.Y / width)
                let z = MathF.roundToInt (position.Z / width)

                let optDistance =
                    let pos = struct (x, y, z)
                    match this.CachedDistances.TryGetValue(pos) with
                    | true, optDistance -> optDistance
                    | _ ->
                        let distance =
                            this.Sdf.GetDistance(Vector3(float32 x * width, float32 y * width, float32 z * width))
                            - this.HalfDiagonal
                        
                        let optDistance =
                            if distance > this.Epsilon
                            then distance
                            else Single.NaN
                        this.CachedDistances.TryAdd(pos, optDistance) |> ignore
                        optDistance

                if Single.IsNaN(optDistance) then
                    this.Sdf.GetDistance(position)
                else
                    optDistance

    let cache (width:float32) (epsilon:float32) (sdf:ISignedDistanceField) =
            Helper.construct<Cache<_>> [|
                box width
                box (sqrt (width * width * 3f) * 0.5f)
                box epsilon
                box (System.Collections.Concurrent.ConcurrentDictionary<struct (int * int * int), float32>())
                box sdf
            |] :?> ISignedDistanceField

    [<Struct>]
    type private Measure<'sdf
        when 'sdf :> ISignedDistanceField
        > =
        {
            Stopwatch : System.Diagnostics.Stopwatch
            mutable Sdf : 'sdf
        }

        interface ISignedDistanceField with
            member this.GetDistance(position) =
                this.Stopwatch.Start()
                try
                    this.Sdf.GetDistance(position)
                finally
                    this.Stopwatch.Stop()

    let measure (stopwatch) (sdf:ISignedDistanceField) =
            Helper.construct<Measure<_>> [|
                box stopwatch
                box sdf
            |] :?> ISignedDistanceField

module Test =
    let trace (epsilon:float32) (length:float32) (sdf:ISignedDistanceField) (ray:Ray) =
        let direction = ray.Direction
        let position = ray.Position

        let rec test (position:Vector3) (length:float32) =
            let distance = sdf.GetDistance(position)
            if distance < epsilon then
                ValueSome (position, distance)
            elif length <= 0f then
                ValueNone
            else
                test (position + direction * distance) (length - distance)

        let distance = sdf.GetDistance(position)
        test (position + direction * distance) length

    let normal (epsilon:float32) (sdf:ISignedDistanceField) (position:Vector3) =
        let inline f (dimension:Vector3) =
            let epsilon = dimension * epsilon
            sdf.GetDistance(position + epsilon) - sdf.GetDistance(position - epsilon)

        Vector3(f Vector3.UnitX, f Vector3.UnitY, f Vector3.UnitZ)
        |> Vector3.normalized

    let normalFAST (epsilon:float32) (sdf:ISignedDistanceField) (position:Vector3) (distanceAtPosition:float32) =
        Vector3(
            sdf.GetDistance(position + Vector3.UnitX * epsilon) - distanceAtPosition,
            sdf.GetDistance(position + Vector3.UnitY * epsilon) - distanceAtPosition,
            sdf.GetDistance(position + Vector3.UnitZ * epsilon) - distanceAtPosition
        )
        |> Vector3.normalized

    let traceWithDirectionalLigth (epsilon:float32) (length:float32) (sdf:ISignedDistanceField) (lightDirection:Vector3) =
        let inline trace (ray) = trace epsilon length sdf ray
        let normalEpsilon = epsilon / 1.0f
        let inline normal (position) (distance) = normalFAST normalEpsilon sdf position distance

        let lightDirection = -lightDirection
        fun (ray:Ray) ->
        match  trace ray with
        | ValueSome (position, distance) ->
            let normal = normal position distance
            let light =
                let dot = Vector3.dot normal lightDirection

                if dot <= 0f then
                    0f
                else
                    // shadow
                    trace {
                        Position = position + normal * epsilon
                        Direction = lightDirection
                    } |> function
                    | ValueNone -> dot
                    | ValueSome _ -> 0.0f

            0.1f + light * 0.9f
        | ValueNone -> 0.0f
