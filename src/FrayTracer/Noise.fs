namespace FrayTracer.Core

open System
open Spline

module Noise =
    let private permutationTable = [|
        225; 155; 210; 108; 175; 199; 221; 144; 203; 116;  70; 213;  69; 158;  33; 252;
          5;  82; 173; 133; 222; 139; 174;  27;   9;  71;  90; 246;  75; 130;  91; 191;
        169; 138;   2; 151; 194; 235;  81;   7;  25; 113; 228; 159; 205; 253; 134; 142;
        248;  65; 224; 217;  22; 121; 229;  63;  89; 103;  96; 104; 156;  17; 201; 129;
         36;   8; 165; 110; 237; 117; 231;  56; 132; 211; 152;  20; 181; 111; 239; 218;
        170; 163;  51; 172; 157;  47;  80; 212; 176; 250;  87;  49;  99; 242; 136; 189;
        162; 115;  44;  43; 124;  94; 150;  16; 141; 247;  32;  10; 198; 223; 255;  72;
         53; 131;  84;  57; 220; 197;  58;  50; 208;  11; 241;  28;   3; 192;  62; 202;
         18; 215; 153;  24;  76;  41;  15; 179;  39;  46;  55;   6; 128; 167;  23; 188;
        106;  34; 187; 140; 164;  73; 112; 182; 244; 195; 227;  13;  35;  77; 196; 185;
         26; 200; 226; 119;  31; 123; 168; 125; 249;  68; 183; 230; 177; 135; 160; 180;
         12;   1; 243; 148; 102; 166;  38; 238; 251;  37; 240; 126;  64;  74; 161;  40;
        184; 149; 171; 178; 101;  66;  29;  59; 146;  61; 254; 107;  42;  86; 154;   4;
        236; 232; 120;  21; 233; 209;  45;  98; 193; 114;  78;  19; 206;  14; 118; 127;
         48;  79; 147;  85;  30; 207; 219;  54;  88; 234; 190; 122;  95;  67; 143; 109;
        137; 214; 145;  93;  92; 100; 245;   0; 216; 186;  60;  83; 105;  97; 204;  52; |]

    let private perm x =
        permutationTable.[ x &&& permutationTable.Length-1 ]

    let private index ix iy iz =
        perm (ix + (perm (iy + perm iz)))

    let private rand = Random(666)

    let private valueTable = Array.init (permutationTable.Length) (fun i -> 2.0 * rand.NextDouble() - 1.0)
                   
    let private vLattice ix iy iz =
        valueTable.[index ix iy iz]

    let value x y z =
        let idx = [| floor x; floor y; floor z |]
        let fl  = [| x - idx.[0]; y - idx.[1]; z - idx.[2] |]
    
        let xKnots = Array.zeroCreate (4)
        let yKnots = Array.zeroCreate (4)
        let zKnots = Array.zeroCreate (4)

        for k in -1 .. 1 .. 2 do
            for j in -1 .. 1 .. 2 do
                for i in -1 .. 1 .. 2 do
                    xKnots.[i+1] <- vLattice (int idx.[0] + i) (int idx.[1] + j) (int idx.[2] + k)
                yKnots.[j+1] <- catmulRom1D (xKnots) (fl.[0])
            zKnots.[k+1] <- catmulRom1D (yKnots) (fl.[1])
    
        catmulRom1D (zKnots) fl.[2]         

(*
    let indexA (idx:array<int>) =
        idx
        |> Array.rev
        |> Array.fold ( fun acc elem -> (perm acc) + elem ) idx.[0]

    let vLatticeA idx =
        valueTable.[indexA idx]

    let valueNA (crds:array<float>) =
        let idx   = crds |> Array.map ( floor )
        let nrm   = crds |> Array.mapi ( fun i x -> x - idx.[i] )
        let knots = crds |> Array.map  ( fun x -> (Array.init (4) (fun i -> 0.0)) )       
        ()
*)


    let private gradientTable = Array.init (permutationTable.Length) (fun i ->
        let z = rand.NextDouble() * 2.0 - 1.0
        let r = sqrt (1.0 - z*z)
        let theta = 2.0 * Math.PI * rand.NextDouble()
        ( r * cos (theta), r * sin(theta), z ) )

    let private gLattice ix iy iz fx fy fz =
        let x,y,z = gradientTable.[index ix iy iz]
        x*fx + y*fy + z+fz

    let private lerp t x0 x1 = x0 + t*(x1-x0)
    let private smoothStep x = x*x * (3.-2.*x)

    let private prepValues x =
        let ix = int (floor x) 
        let fx0 = x - float ix
        let fx1 = fx0 - 1.0
        let wx = smoothStep fx0
        (ix, fx0, fx1, wx)
    
    let private latAndLerp w ix0 iy0 iz0 fx0 fy0 fz0 ix1 iy1 iz1 fx1 fy1 fz1 =
        let a = gLattice ix0 iy0 iz0 fx0 fy0 fz0
        let b = gLattice ix1 iy1 iz1 fx1 fy1 fz1
        lerp w a b

    let gradient x y z =
        let ix, fx0, fx1, wx = prepValues x
        let iy, fy0, fy1, wy = prepValues y
        let iz, fz0, fz1, wz = prepValues z

        let vy0 = latAndLerp wx ix iy iz fx0 fy0 fz0 (ix+1) iy iz fx1 fy0 fz0
        let vy1 = latAndLerp wx ix (iy+1) iz fx0 fy1 fz0 (ix+1) (iy+1) iz fx1 fy1 fz0
        let vz0 = lerp wy vy0 vy1

        let vy0 = latAndLerp wx ix iy (iz+1) fx0 fy0 fz1 (ix+1) iy (iz+1) fx1 fy0 fz1
        let vy1 = latAndLerp wx ix (iy+1) (iz+1) fx0 fy1 fz1 (ix+1) (iy+1) (iz+1) fx1 fy1 fz1
        let vz1 = lerp wy vy0 vy1

        lerp wz vz0 vz1
        
    let valueGradient x y z =
        (value x y z) + (gradient x y z)
