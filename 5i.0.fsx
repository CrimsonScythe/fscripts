(*isTable*)
(*The function takes a type: 'a list list checks whether it is a table. It returns either true or false.*)

///<summary>
/// The function takes a list of lists ('a list list) as input and provided that it is not empty, it checks if all lists have equal length
/// and that there is atleast one list with atleast 1 element. If this is true then it returns true, otherwise false.
///</summary>
///<params name="x">
/// The input argument.
///</params>
///<returns>
/// A mutable boolean with initial value of false.
/// result becomes true if the list of lists is not empty and all lists have equal length.
/// Otherwise it is false. 
///</returns>
/// <remarks>
/// the code is tested in the printfn() function below.
/// </remarks>
let isTable (x : 'a list list):bool=
    let l = x.[0].Length
    let mutable result = false
    if x.Length >= 1 && not x.[0].IsEmpty then
        result<-true
        for elm in x do
            if elm.Length = l then
                result<-true
            else
                result<-false
        done
    result
  
printfn "a) %b" ((isTable [[1;2;3];[4;5;6]])=true)
printfn "a) %b" ((isTable [[]])=false)
printfn "a) %b" ((isTable [[1;2;3]; [5;6]])=false)
printfn "a) %b" ((isTable [[4;5;6]; [8;1;1]; [3;6;9]])=true)

(*firstColumn*)
(*The function takes a type: 'a list list and returns a new list containing the first elements of each list in the list of lists.*)

///<summary>
/// The function takes a list of lists ('a list list) as input and uses a variable to store the first element of each list in the list of lists.
/// It returns this new list, which is the first column of the table.
///</summary>
///<params name="x">
/// The input argument, a list of lists.
///</params>
/// ///<example>
/// If the input list is not empty and has length atleast 1 the for loop runs.
/// <code>
///     if x.Length >= 1 && not x.[0].IsEmpty then
///        for elm in x do
///            a <- elm.[0]::a
///        done
/// </code>
/// The for loop uses string indexing to retrieve the the 0th element of each list present in x.
/// the cons operator :: is used to add the element to list a which is returned.
/// The for loop runs from 0 to x.Length-1, so it runs for the total number of lists present in the input list of lists.
/// </example>
///<returns>
/// A mutable variable that is an empty list. The values extracted from the list of lists in the for loop is added to this empty list.
///</returns>
///<remarks>
/// The return statement uses the function List.rev() to reverse the order of the final list since the cons operator places each element in front everytime it runs.
/// 
/// The function is tested in the printfn function below.
///</remarks>
let firstColumn (x : 'a list list)=
    let mutable a=[]
    if x.Length >= 1 && not x.[0].IsEmpty then
        for elm in x do
            a <- elm.[0]::a
        done
    List.rev a
printfn "%s" "----------"
printfn "b) %b" ((firstColumn [[1;2;3]; [4;5;6]])=[1;4]) 
printfn "b) %b" ((firstColumn [[1;2;3]; [4;5;6]; [5;9;8]])=[1;4;5]) 
printfn "b) %b" ((firstColumn [[1;2;3]])=[1]) 
printfn "b) %b" ((firstColumn [[]])=[]) 

(*dropFirstColumn*)
(*The function takes a type: 'a list list and returns a table of type: 'a list list by dropping the first column of the table.*)

///<summary>
/// The function takes a list of lists ('a list list) as input and uses a for loop and string slicing to extract all elements except the first (0th index). 
///</summary>
///<example>
/// The for loop:
/// <code>
///        for elm in x do
///            let ll = elm.Length-1
///            a <- elm.[1..ll]::a
///        done
/// </code>
/// Uses string slicing from index 1 to the length of the list to extract all elements except the first. This is stored in the mutable variable a.
/// The for loop runs from 0 to x.Length-1, so it runs for the total number of lists in the input list of lists.
/// </example>
///<params name="x">
/// The input argument, a list of lists.
///</params>
///<returns>
/// A mutable variable that is an empty list. The values extracted from the list of lists in the for loop is added to this empty list.
///</returns>
///<remarks>
/// The return statement uses the function List.rev() to reverse the order of the final list since the cons operator places each element in front everytime it runs.
/// 
/// The function is tested in the printfn() function below.
///</remarks>
let dropFirstColumn (x : 'a list list)=
    let mutable a=[]
    if x.Length >= 1 && not x.[0].IsEmpty then
        for elm in x do
            let ll = elm.Length-1
            a <- elm.[1..ll]::a
        done
    List.rev a

printfn "%s" "----------"
printfn "c) %b" ((dropFirstColumn [[1;2;3]; [4;5;6]])=[[2;3];[5;6]]) 
printfn "c) %b" ((dropFirstColumn [[2;3]; [5;6]])=[[3];[6]]) 
printfn "c) %b" ((dropFirstColumn [[]])=[]) 
printfn "c) %b" ((dropFirstColumn [[-1;-5;-6]; [-1;-8;-9]; [0;0;0]])=[[-5;-6];[-8;-9];[0;0]]) 

(*transpose*)
(*The function takes a type: 'a list list and returns a table of type: 'a list list by converting the columns from the input list to rows in the output list.*)

///<summary>
/// The function takes a list of lists ('a list list) as input and uses the functions dropFirstColumn and FirstColumn defined above, to transpose the input list.
///</summary>
///<params name="x">
/// The input argument, a list of lists.
///</params>
/// </example>
/// The code below inserts the first column of the input list into c using the append function.
/// <code>
/// c <- c@[first]
/// </code>
/// The for loop:
/// <code>
///    for elm in remainder do
///        first <- firstColumn remainder
///        remainder <- dropFirstColumn remainder 
///        if not remainder.IsEmpty then
///            c <- c@[first]
/// </code>
/// The first column is extracted from the remainder list and then appended to the output list c.
/// Since the first column has been copied, the dorpfirst column is used to discard it.
/// The firstColumn function is then called again on the remainder list.
/// This is done until the firstColumn function returns empty or if all columns have been copied.
/// 
/// The if statement checks whether the remiander is not empty. This is used so that an empty list [] 
/// is not added to c.
/// 
/// The for loop runs until the first list is empty or until there are no more columns left to copy.
/// 
/// </example>
///<returns>
/// variable c
///</returns>
///<remarks>
/// The function can be tested using the prinfn() fuunction below.
///</remarks>

let transpose (x : 'a list list)=
    let mutable c = []
    let mutable first = firstColumn x
    let mutable remainder = dropFirstColumn x
    c <- c@[first]

    for elm in remainder do
        first <- firstColumn remainder
        remainder <- dropFirstColumn remainder 
        if not remainder.IsEmpty then
            c <- c@[first]
    c


printfn "%s" "----------"
printfn "d) %b" ((transpose [[1;2;3]; [4;5;6];])=[[1;4];[2;5];[3;6]])
printfn "d) %b" ((transpose (transpose [[1;2;3]; [4;5;6]]))=[[1;2;3];[4;5;6]])
printfn "d) %b" ((transpose [[4;3]; [2;0];])=[[4;2];[3;0]])
printfn "d) %b" ((transpose [[4;3]; [2;0];])=[[4;2];[3;0]])
printfn "d) %b" ((transpose [[2;4;6;8]; [3;6;8;15]; [14;12;0;2]])=[[2;3;14];[4;6;12];[6;8;0];[8;15;2]])


