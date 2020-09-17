/// Opgave 4g2a

open System
let polyLen (n : int): float=
    let vector1 = (Math.Cos((2.0*Math.PI)/(float)n), Math.Sin((2.0*Math.PI)/(float)n))
    let vector2 = vec2d.scale(-1.0) (1.0, 0.0)
    let vector3 = vec2d.add(fst(vector1),snd(vector1)) (fst(vector2), snd(vector2))
    vec2d.len(fst(vector3), snd(vector3)) * (float)n

printfn "%-20s %-20s %-20s" "n" "Længden" "Resultat"                                //Gives three headings to the table
let mutable resultat = 0.0                                                          //mutable variable 
for i = 2 to 30 do                                                                  //We test polyLen() for values from 2 till 30 
resultat <- (2.0*Math.PI) - (polyLen i)                                             //difference between polygon and circle parameter       
printfn "%-20s %-20s %-20s" (i.ToString()) ((polyLen i).ToString()) (resultat.ToString()) //creates a row with three values each time
done 

///<summary>  
/// The function "polyLen ()" takes a integer n value and return as a float value.  
/// The function uses the library (vec2d) from 4g0.fs to calculate the length of a polygon. 
/// The lenght is calculated as the sum of the vectors between neighbor points,
/// where n denotes the number of times where the polygon is touching the circle (with radius 1)
/// 
///<summary> 
///<params name = "vector1">  
/// Calculates the x and y values for the required polygon vector. The x value is calculated using cos and the y value using sin.
/// The angle is calcualted using the formula: (2*PI)/n where n is the number of sides of the polygon.
///</params>
///<params name = "vector2">  
/// Uses the Scale() function to change the direction of the x-axis vector. 
/// This enables us to easily use the Add() function with vector3.
/// </params>
///<params name = "vector3">  
/// Uses the Add() function to return a new vector (Vector3) which is the sum of vector 1 and vector 2.
/// Vector3 is the polygon side.
/// </params>
/// <returns>
/// vec2d.len() is used to calculate the length of the polygon side. This is multiplied by n to get the value for the whole polygon.
/// </returns>
/// <remarks>
/// We used casting (float) to convert the n to a float in polyLen(). 
/// </remarks>
/// 
/// 
///</...> 

/// Opgave 4g2b
/// Hypothesis: If n → ∞ it will mean that the difference between the circumference of the polygon and the circumference of the circle becomes smaller.
/// This will mathematically mean that the circumference of the polygon → circumference of the circle, when n → ∞
/// This is because of the more points where the polygon touches the circle, the more vectors with a shorter distance to each other will appear.
/// If there are more vectors with a shorter distance between each other, it will mean that the polygon sides will approach the circles. 