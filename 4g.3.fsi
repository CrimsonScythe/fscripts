/// Opgave 4g3

/// A 2 dimensional vector library with floats and lists instead of tuples.
module vec2d

val len : float -> float -> float

val ang : float -> float -> float

val scale : float -> float -> float -> float list

val add : float -> float -> float -> float -> float list

val dot : float -> float -> float -> float -> float

/// In the len(), ang() and dot() functions, we change the tuple to two separate floats. The first and second floats are the inputs
/// and the last float is the output.
/// Similraly, in the scale() and add() functions we change the input tuples to floats. To avoid using a tuple in the output 
/// we use a float list type.
/// 
///(1) The input types in the functions in the implementation file need to be changed so they don't use tuples.
///(2) The program would become less user friendly since it is easier to make mistakes when inputting values for the functions.
/// For example, the modified function scale() would take 3 input arguments, the first will be the scale factor, 
/// the second will be the x value of the vector and the third will be the y value of the vector. 
/// The user might pass the vector values as the first and second arguments and the scale factor as the third. 
/// This would give a wrong value.  <3 <3 <3   