(* Opgave [6g]
 *
 * Hold: 4
 * Gruppe: 3 
 *   Nikolai 
 *   Haseeb
 *   Jonatan
 *) 

/// Assignment 6g.0

///<summary>
/// The function "cfrac2float" takes a list of integers presented as a continued 
/// fraction and calculates the corresponding real number.
/// The function is based on the knowledge of continued fractions, which says;
/// that the real number for a continued fraction can be calculated as 
/// x = [q0;q1,q2,...],. 
/// The function uses recursion to calculate the real number, where the function
/// first checks if the length of the given list is equal to 1 (the list contains
/// only 1 element). If so, the function returns the float value for that element. 
/// If not, the function finds the float value for the first element in the list
/// and then adds 1.0 divided by the next elements in the list. 
/// Therefore, the function uses recursion and the knowledge for the real number 
/// for a continued fraction to find the real number for a continued fraction. 
///</summary>
///<params name = "lst">
/// The param "lst" is giving to the function as a list of integers. 
/// The param is used first in the "if" statement, where the function checks if 
/// the length of the param is equal to 1. Afterwards, the param is used in the
/// "else" statement to calculate the real number with recursion. 
///<returns>
/// The function "cfrac2float" is giving a list of integers and returns the 
/// real number for the continued fraction. The function returns a float value,
/// because the real number for a continued fraction is mostly a float value. 
/// The function has been tried with different "lst" values,
/// printed as a "%b" (bool value) to check if the result is the same, as the 
/// result that we were expecting to get. 

let rec cfrac2float (lst: int list) : float =
    if lst.Length = 1 then float(lst.[0])
    else float(lst.Head) + (1.0/(cfrac2float lst.Tail))
    
printfn "%b" (cfrac2float [3;4;12;4] = 3.245)
printfn "%b" (cfrac2float [3;4] = 3.25) 
printfn "%b" (cfrac2float [10;5;0] = 10.00)
printfn "%b" (cfrac2float [10] = 10.00)

///</...> 