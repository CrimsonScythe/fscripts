(*gennemsnit*)
(*The function takes a float list and returns the average of the list if it is well-defined. Otherwise it returns null*)

///<summary>
/// The List.fold() function is used to return the average of the float list.
///</summary>
///<params name="x">
/// The input argument float list.
///</params>
///<returns>
/// result
/// OR
/// null
///</returns>
/// 
let gennemsnit (x : float list)=
    if x.Length > 0 then
        let add acc elm = acc + elm
        let result = List.fold add 0.0 x
        Some (result/(float)x.Length)
    else 
        None

printfn "%b" ((gennemsnit [2.0; 3.0 ; 5.0])= Some ((2.0+3.0+5.0)/3.0)) 
printfn "%b" ((gennemsnit [])= None) 
printfn "%b" ((gennemsnit [0.0])=Some 0.0) 
printfn "%b" ((gennemsnit [-2.0; -1.0; -3.0])= Some ((-2.0-1.0-3.0)/3.0)) 
printfn "%b" ((gennemsnit [10.0; 2.0; 13.0])= Some ((10.0+2.0+13.0)/3.0)) 

