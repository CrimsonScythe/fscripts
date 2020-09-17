/// Opgave 4g0

module vec2d
open System

(*Length*)
(*The function takes a tuple and returns a float*)

///<summary>
///By taking the square root of the x- and y- values squared of a vector, 
/// and returns the length of the vector.
///</summary>
///<params name="x">
///The x-value of the vector 
///</params>
///<params name="y">
///The y-value the vector
///</params>
///<returns>
///The function returns the length of the vector.
///</returns>
let len (x: float, y: float) : float=
    sqrt(x**2.0 + y**2.0)


(*Angle*)
(*The function takes a tuple and reaturns a float*)

///<summary>
///By calling Math.Atan2 from the library, it returns the angle of the vector.
///</summary>
///<params name="x">
///The x-value of the vector.
///</params>
///<params name="y">
///The y-value of the vector.
///</params>
///<returns>
///The function returns the angle of the vector.
///</returns>
let ang (x: float, y: float) : float=
    Math.Atan2(y, x)


(*Scale*)
(*The function takes a float, a tuple and returns a tuple*)

///<summary>
///By multiplying the x- and y-values with constant a, 
/// it scales the vector.
///</summary>
///<params name="a">
///The constant, by witch the vector is scaled
///</params>
///<params name="x">
///The x-value of the vector
///</params>
/// ///<params name="y">
///The y-value of the vector
///</params>
///<returns>
///The function returns the scaled vector by the constant
///</returns>
let scale (a: float) (x: float, y :float)=
    (a*x, a*y)



(*Add*)
(*The function takes two tuples and returns a tuple*)

///<summary>
///The function serves to find the polygon vector of the figure.
///</summary>
///<params name="x">
///The x-value of the vector.
///</params>
///<params name="y">
///The y-value of the vector.
///</params>
/// ///<params name="x1">
///The x-value of the second vector.
///</params>
///<params name="y2">
///The y-value of the second vector.
///</params>
///<returns>
///The function finds the length of the third vector.
///</returns>
///<remarks>
///The second vector is used here, adding it to the first, 
/// to produce the third.
///</remarks>
let add (x: float, y: float) (x1: float, y1: float)=
    (x + x1, y + y1)


(*Dot*)
(*The function takes two tuples and returns a float*)

///<summary>
///The function finds the dot product of the first and 
/// second vector.
///</summary>
///<params name="x">
///The x-value of the first vector.
///</params>
///<params name="y">
///The y-value of the first vector.
///</params>
/// ///<params name="x1">
///The x-value of the second vector.
///</params>
///<params name="y1">
///The y-value of the second vector.
///</params>
///<returns>
///The function returns the dot product of the two vectors.
///</returns>
///<remarks>
///The second vector is used here.
///</remarks>
let dot (x: float, y: float) (x1: float, y1: float)=
    x*x1 + y*y1

///</...>

