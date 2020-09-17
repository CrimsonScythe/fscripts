/// Assignment 6g.1

///<summary>
/// The function "float2cfrac" does the opposite of the function "cfrac2float"
/// Explanatory, this function is giving the real number and calculates the 
/// numbers representation as a continued fraction for a giving real number.   
/// The function is based on the knowledge for continued fractions, which says
/// that the continued fraction for a real number can be calculated with the 
/// algorithm: For x0 = x and i>1 do calculate q1 = [x1], r1 = x1 - q1 and
/// x(i+1) = 1/r1 until r(i) = 0.
/// The function is using recursion to calculate the continued fraction for a 
/// real number. At first, the function uses the system to find the Math.Round 
/// for the float value, which means that it is rounds down the number.
/// Afterwards it takes the original x value and minuses it with the 
/// downrounded number of it selfs. If the tuple for the value of (r,1) is 
/// equal to 1 the function returns the integer value for the q. 
/// If not, the function uses recursion to calculate the continued fraction 
/// for a real number, where it takes the integer value for the real number: 
/// makes it to a list, and the adds the same (1.0/r) which will continue
/// until r = 0. Therefor, the function is using the knowledge for continued
/// fractions and recursion to find the continued fraction for a real number.
///</summary>
///<params name = "x">
/// This param is giving to the function as a float value. 
/// The float value is represented as the real number. 
/// The param is first used to define the q value, which is the down rounded x.
/// Afterwards, the param is used to the calculate the continued fraction 
/// for the param, where the function uses recursion in the calculation. 
///<returns>
/// The function is giving a float value "x" and returns a list of integers. 
/// The float value is a real number, and the functio returns a list 
/// The list is the real number's continued fraction. 
/// The function has been tried with different "x" values, printed as a
/// "%b" (bool value) to check if the result is the same, as the 
/// result that we were expecting to get. 

open System
let rec float2cfrac (x : float) : int list =
    let q = Math.Round x
    let r = (x - q)
    if Math.Round (r,1) = 0.0 then [int(q)]
    else [int(q)] @ (float2cfrac (1.0/r))

printfn "%b" (float2cfrac 3.245 = [3; 4; 12; 4])
printfn "%b" (float2cfrac 10.00 = [10])
printfn "%b" (float2cfrac -5.25 = [-5; -4])
printfn "%b" (float2cfrac 372.17 = [372; 6; -8; -2])
printfn "%b" (float2cfrac 17.25 = [17; 4])

///</...> 