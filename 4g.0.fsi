/// A 2 dimensional vector library .
/// Vectors are represented as pairs of floats
module vec2d
/// The length of a vector
val len : float * float -> float
/// The angle of a vector
val ang : float * float -> float
/// Multiplication of a float and a vector
val scale : float -> float * float -> float * float
/// Addition of two vectors
val add : float * float -> float * float -> float * float
/// Dot product of two vectors
val dot : float * float -> float * float -> float
