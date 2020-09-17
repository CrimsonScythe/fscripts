/// Assignment 6g.3

///<summary>
/// The function "cfrac2frac" is giving a list and a integer. The list is in
/// the knowledge for continued fraction the continued fraction and the integer
/// is an index. The function calculates the t(i)/n(i) approximation as
/// int * int. The int * int is a tuple with (ti,ni). The function uses the
/// knowledge for  finding the approximation of a continued fraction as a 
/// integerfraction t(i)/n(i), where i >= 0 which says that: 
/// t(i) = q(i) * t(i-1) + t(i-2) and n(i) = q(i) * n(i-1) + n(i-2). 
/// The function uses recursion to calculate the result, where the function 
/// "cfrac2frac" is defined at the beginning. Aftwerwards, the function calls
/// itself as long as i ≠ 0. If i = 1 the function takes the number i'th 
/// element from the list, and uses recursion to the same for the tuple
/// ((lst 0))+1,lst[i]. This means that the function wille stop, when i = 0.
/// If i > 1, then the function creates two new variables "rrr" and "kkk".
/// Afterwards, the function uses recursion for the first and second elements
/// for the list where, where now the list elements are either rrr or kkk. 
/// This is used to create the tuple for (t(i),n(i)). 
///</summary>
///<params name = "lst">
/// This param is giving to the function as a list of integers. This list is a 
/// representation for the continued fraction. The param is used in all the steps
/// in the function, where the param mostly are used in the recursion steps. 
///<params name = "i">
/// This param is giving to the function as a integer value. This integer is a 
/// representation for the index. The param is used in all the steps in the 
/// function, both for the if, else if and else steps, and also in the 
/// recursion steps of the function. 
///<returns>
/// The function is giving a list and a integer, that representates the
/// continued fraction and the index. The function is giving the two 
/// parameters and returns a tuple (int * int). This tuple representates the 
/// the integerfraction t(i)/n(i) for a continued fraction with and index. 
/// The function has been tried with different values for both parameters. 
/// All the tests is printing a bool value that compares the results with 
/// teh results that we expected to get. 

let rec cfrac2frac (lst : int list) (i : int) : int * int =
    if i = 0 then ((lst.[i]), 1)
    elif i = 1 then ((lst.[i] * fst((cfrac2frac lst 0))+1), (lst.[i]))
    else 
        let rrr = i-1
        let kkk = i-2
        ((lst.[i] * fst((cfrac2frac lst rrr)) + fst((cfrac2frac lst kkk))) , ((lst.[i] * snd((cfrac2frac lst rrr)) + snd((cfrac2frac lst kkk)))))
     

printfn "%b" (cfrac2frac [3;4;12;4] 0 = (3, 1))
printfn "%b" (cfrac2frac [3;4;12;4] 1 = (13, 4))
printfn "%b" (cfrac2frac [3;4;12;4] 2 = (159, 49))
printfn "%b" (cfrac2frac [3;4;12;4] 3 = (649, 200))
printfn "%b" (cfrac2frac [10;5;0] 0 = (10, 1))
printfn "%b" (cfrac2frac [10;5;0] 1 = (51, 5))
printfn "%b" (cfrac2frac [10;5;0] 2 = (10, 1))
printfn "%b" (cfrac2frac [3;4] 1 = (13, 4))

///</...> 