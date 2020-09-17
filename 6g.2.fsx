/// Assignment 6g.2

///<summary>
/// The function "frac2cfrac" takes two integers and calculates the two
/// integers representation as a continued fraction only by using integers. 
/// The two integers that are giving to the function is the numerator and
/// the mention in a fraction, where it is a integerfraction. To find the 
/// continued fraction for a intergerfraction, the function is using the 
/// knowledge for integerfractions to continued fractions, which uses integer-
/// division and rest to calculate the continued fraction. The rules the 
/// function are using is that: if a = t % n is the integerdivision between 
/// t and n, and b = t % n is the rest for the integerdivision, then t = a*n+b.
/// The function uses recursion to calculate the continued fraction for 
/// a integerdivision. Explanatory, the function first define a r value that 
/// is the rest between the numerator and the denominator in the integerfraction. 
/// Afterwards, the function define a q value that is the difference between 
/// the numerator integer and the rest value divided by the denominator integer. 
/// If the rest is equal to 0 the function returns the q value, if not
/// the function uses recursion to calculate the continued fraction, where the
/// function continues the to find the rest between the integers until the rest 
/// is equal to 0. Therefor, the function uses recursion and the knowledge for 
/// a integerfraction to calculate the mentioned fraction for a integerfraction.
///</summary>
///<params name = "t">
/// This param is giving to the function as a integer value. This integer is 
/// represented as the numerator for the integerfraction t/n. The param is used 
/// first to define the r value (the rest) and then the define the q value. 
///<params name = "n">
/// This param is giving to the function as a integer value. This integer is 
/// represented as the denominator for the integerfraction t/n. The param is 
/// used first to define the r value (the rest) and then the define the q value. 
///<returns>
/// The function is giving two integers values and returns a integer list. 
/// The two integer values "t" and "n" are respectively the numerator and
/// the denominator for a fraction. The list of integers that the function 
/// returns is the fractions representation as continued fraction. 
/// The function has been tried with different value for both parameters,
/// printed as a bool value, where we compare the results of function with the 
/// results we expected to get. 


let rec frac2cfrac (t : int) (n : int) : int list = 
    let r = t % n
    let q = (t - r)/n
    if r = 0 then [q]
    else [q] @ (frac2cfrac n r)

printfn "%b" (frac2cfrac -123 40 = [-3; -13; -3])
printfn "%b" (frac2cfrac 563 250 = [2; 3; 1; 30; 2])
printfn "%b" (frac2cfrac 498 212 = [2; 2; 1; 6; 2; 2])
printfn "%b" (frac2cfrac 649 200 = [3;4;12;4])
printfn "%b" (frac2cfrac 422 -51 = [-8;-3;-1;-1;-1;-4])

///</...> 