open System
//Unit: Length
let Length (x : float, y : float) = vec2d.len(x, y) (* WB: 0 *)
//Unit: Angle
let Angle (x : float, y : float) = vec2d.ang(x, y) (* WB: 0 *)
//Unit: Scale
let Scale (a: float) (x: float, y :float) = vec2d.scale(a) (x, y) (* WB: 0 *)
//Unit: Add
let Add (x: float, y: float) (x1: float, y1: float) = vec2d.add(x, y) (x1, y1) (* WB: 0 *)
//Unit: Dot
let Dot (x: float, y: float) (x1: float, y1: float)= vec2d.dot(x, y) (x1, y1) (* WB: 0 *)

printfn "White-box testing of 444.fsx"
printfn "   Unit: Length"
printfn "    Branch: 0 - %b" (Length(2.0, 2.0) = sqrt(8.0))
printfn "    Branch: 0 - %b" (Length(0.0, 0.0) = sqrt(0.0))
printfn "    Branch: 0 - %b" (Length(-1.0, -1.0) = sqrt(2.0))
printfn "   Unit: Angle"
printfn "    Branch: 0 - %b" (Angle(2.0, 2.0) = Math.Atan2(2.0,2.0))
printfn "    Branch: 0 - %b" (Angle(0.0, 0.0) = Math.Atan2(0.0,0.0))
printfn "    Branch: 0 - %b" (Angle(-1.0, -1.0) = Math.Atan2(-1.0,-1.0))
printfn "   Unit: Scale"
printfn "    Branch: 0 - %b" (Scale(2.0) (2.0 , 2.0) = (2.0*2.0,2.0*2.0))
printfn "    Branch: 0 - %b" (Scale(2.0) (-1.0 , -1.0) = (2.0* -1.0,2.0* -1.0))
printfn "    Branch: 0 - %b" (Scale(2.0) (0.0 , 0.0) = (2.0*0.0,2.0*0.0))
printfn "   Unit: Add"
printfn "    Branch: 0 - %b" (Add(2.0, 2.0) (3.0 , 3.0) = (5.0,5.0))
printfn "    Branch: 0 - %b" (Add(2.0, 2.0) (-1.0 , -1.0) = (1.0,1.0))
printfn "    Branch: 0 - %b" (Add(2.0, 2.0) (0.0 , 0.0) = (2.0,2.0))
printfn "   Unit: Dot"
printfn "    Branch: 0 - %b" (Dot(2.0, 2.0) (3.0 , 3.0) = (12.0))
printfn "    Branch: 0 - %b" (Dot(2.0, 2.0) (0.0 , 0.0) = (0.0))
printfn "    Branch: 0 - %b" (Dot(2.0, 2.0) (-1.0 , -1.0) = (-4.0))
