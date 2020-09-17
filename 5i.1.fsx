(*concat*)
(*The function takes a list of lists and extracts each element from each list to return a concatenated list of all elements present.*)

///<summary>
/// The List.collect function is used to extract the elements within each list.
/// It is used twice becuase the first time it iterates through each list present in the list of lists
/// and the second time it iterates through each element present in the list.
///</summary>
///<params name="y">
/// The input argument list of lists ('a list list).
///</params>
///<params name="concatenatedList">
/// A mutable variable that holds teh result from the List.collect (List.collect ()) functions.
///</params>
///<returns>
/// concatenatedList
///</returns>
/// /// </example>
/// <code>
/// let mutable concatenatedList = List.collect (fun z -> List.collect (fun c -> [c]) z) y
/// </code>
/// The first List.collect function has an anonymous function defined by: fun z.
/// The body of the anonymous function uses List.collect which has another anonymous function: fun c.
/// The first List.collect iterates through the input y and applies  fun z on each list in y.
/// The second List.collect iterates through each element in the list from fun z and applies fun c to extract each element.
/// The function then returns the output as a concatenated list: 'a list.
/// </example>
let concat (y : 'a list list)=
    let mutable concatenatedList = List.collect (fun z -> List.collect (fun c -> [c]) z) y
    concatenatedList
   
printfn "%b" ((concat [[2]; [6;4]; [1]])=[2;6;4;1])
printfn "%b" ((concat [[]; [6;4]; [1]])=[6;4;1])
printfn "%b" ((concat [[2]; [4]; [1]])=[2;4;1])
printfn "%b" ((concat [[-1]; [15;8]; [1]])=[-1;15;8;1])
printfn "%b" ((concat [[2;5;8;9;7;10]; []; [1]])=[2;5;8;9;7;10;1])



