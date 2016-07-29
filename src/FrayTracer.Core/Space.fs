module FrayTracer.Core.Space

[<Measure>] type m = FSharp.Data.UnitSystems.SI.UnitSymbols.m

type Vector = {
    X : float<m>
    Y : float<m>
    Z : float<m>
    } with

    static member inline op_Addition( a:Vector, b:Vector ) =
        {
        Vector.X = a.X + b.X
        Y = a.Y + b.Y
        Z = a.Z + b.Z
        }

    static member inline op_Subtraction( a:Vector, b:Vector ) =
        {
        Vector.X = a.X - b.X
        Y = a.Y - b.Y
        Z = a.Z - b.Z
        }

    static member inline op_Multiply( a:Vector, b:Vector ) =
        a.X * b.X + a.Y * b.Y + a.Z * b.Z

type Position = {
    X : float<m>
    Y : float<m>
    Z : float<m>
    } with

    static member inline op_Addition( a:Position, b:Vector ) =
        {
        Position.X = a.X + b.X
        Y = a.Y + b.Y
        Z = a.Z + b.Z
        }

    static member inline op_Subtraction( a:Position, b:Position ) =
        {
        Vector.X = b.X - a.X
        Y = b.Y - a.Y
        Z = b.Z - a.Z
        }

type Direction = {
    X : float
    Y : float
    Z : float
    } with

    static member inline op_Multiply( d:Direction, length:float<m> ) =
        {
        Vector.X = d.X * length
        Y = d.Y * length
        Z = d.Z * length
        }

    static member inline op_Multiply( length:float<m>, d:Direction ) = d * length
