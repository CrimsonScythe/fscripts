/// Assignment 6g.4

open System
//Unit: cfrac2float
let rec cfrac2float (lst: int list) : float =

    if lst.Length = 1 then      (* WB: 1 *)
        float(lst.[0]) 
    else                        (* WB: 2 *)
        float(lst.Head) + (1.0/(cfrac2float lst.Tail)) 

printfn "\n White- and black-box testing of 6g.4.fsx"
printfn "   Unit: cfrac2float" 
printfn "       Branch: 1a - %b" (cfrac2float [3;] = 3.0)
printfn "       Branch: 2a - %b" (cfrac2float [3;4;12;4] = 3.245)
printfn "       Branch: 2b - %b" (cfrac2float [3;4] = 3.25) 
printfn "       Branch: 2c - %b \n" (cfrac2float [10;5;0] = 10.00)


//Unit: float2cfrac
let rec float2cfrac (x : float) : int list =
    let q = Math.Round x
    let r = (x - q)

    if Math.Round (r,1) = 0.0 then      (* WB: 1 *)
        [int(q)]
    else                                (* WB: 2 *) 
        [int(q)] @ (float2cfrac (1.0/r))

printfn "   Unit: float2cfrac"
printfn "       Branch: 1a - %b" (float2cfrac 10.00 = [10])
printfn "       Branch: 2a - %b" (float2cfrac 3.245 = [3; 4; 12; 4])
printfn "       Branch: 2b - %b" (float2cfrac -5.25 = [-5; -4])
printfn "       Branch: 2c - %b" (float2cfrac 372.17 = [372; 6; -8; -2])
printfn "       Branch: 2d - %b \n" (float2cfrac 17.25 = [17; 4])

//Unit: frac2cfrac
let rec frac2cfrac (t : int) (n : int) : int list = 
    let r = t % n
    let q = (t - r)/n

    if r = 0 then       (* WB: 1*)
        [q]
    else                (* WB: 2*)   
        [q] @ (frac2cfrac n r)

printfn "   Unit: frac2cfrac"
printfn "       Branch: 1a - %b" (frac2cfrac 10 2 = [5])
printfn "       Branch: 2a - %b" (frac2cfrac -123 40 = [-3;-13;-3])
printfn "       Branch: 2b - %b" (frac2cfrac 563 250 = [2;3;1;30;2])
printfn "       Branch: 2c - %b" (frac2cfrac 498 212 = [2;2;1;6;2;2])
printfn "       Branch: 2d - %b" (frac2cfrac 649 200 = [3;4;12;4])
printfn "       Branch: 2e - %b \n" (frac2cfrac 422 -51 = [-8;-3;-1;-1;-1;-4])

//Unit: cfrac2frac
let rec cfrac2frac (lst : int list) (i : int) : int*int=

    if i = 0 then       (* WB: 1*)
        ((lst.[i]), 1)
    elif i = 1  then    (* WB: 2*)
        ((lst.[i] * fst((cfrac2frac lst 0))+1), (lst.[i]))
    else                (* WB: 3*) 
        let rrr = i-1
        let kkk = i-2
        ((lst.[i] * fst((cfrac2frac lst rrr)) + fst((cfrac2frac lst kkk))) , ((lst.[i] * snd((cfrac2frac lst rrr)) + snd((cfrac2frac lst kkk)))))
     
printfn "   Unit: cfrac2frac"
printfn "       Branch: 1a - %b" (cfrac2frac [3;4;12;4] 0 = (3, 1))
printfn "       Branch: 1b - %b" (cfrac2frac [10;5;0] 0 = (10, 1))
printfn "       Branch: 2a - %b" (cfrac2frac [3;4;12;4] 1 = (13, 4))
printfn "       Branch: 2b - %b" (cfrac2frac [10;5;0] 1 = (51, 5))
printfn "       Branch: 2c - %b" (cfrac2frac [3;4] 1 = (13, 4))
printfn "       Branch: 3a - %b" (cfrac2frac [3;4;12;4] 2 = (159, 49))
printfn "       Branch: 3b - %b" (cfrac2frac [3;4;12;4] 3 = (649, 200))
printfn "       Branch: 3c - %b" (cfrac2frac [10;5;0] 2 = (10, 1))

///</...>