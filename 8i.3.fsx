open ImgUtil
///  * checkColour ()
/// <summary>
/// Checks if figure colour is correct, if range is between 0-255 for each element of the tuple.
/// </summary>
/// <param name="col"> the colour to be checked </param>
/// <returns>true if colour is correct, false otherwise</returns>
/// <remarks>
/// Uses pattern matching and OR statements to check each element of the tuple.
/// type colour is defined in the library files and has type: (int* int *int)
/// </remarks>

let checkColour (col : colour)=
  match col with
    | (a,b,c) ->
        not (a<0 || a>255) || (b<0 || b>255) || (c<0 || c>255)


/// * checkFigure ()
/// <summary>
/// Checks if figure is correct. Top left corners of rectangle should be top right of bottom left corners
/// and Circle radius should be positive. Colour should be in the range 0-255.
/// Uses a helper function checkColour
/// </summary>
/// <param name="fig"> the figure to be checked </param>
/// <returns>true if figure is correct, false otherwise</returns>
/// <remarks>
/// Uses pattern matching to test the figure.
/// the function is called recursively if figure has type: Mix
/// 
/// The function is tested below on various figures.
/// </remarks>

let rec checkFigure (fig : figure) : bool=
 match fig with
  | Circle (( cx , cy ) , r , col ) ->
    match checkColour col with
      | true ->
        not (r<0)
      | false ->
        false

  | Rectangle (( x0 , y0 ) , (x1 , y1 ) , col ) ->
    match checkColour col with
      | true ->
         if x0<=x1 && y0<=y1 then
            true
          else
            false
        // (x0<=x1 && y0<=y1)
      | false ->
        false
   
  | Mix (f1 , f2 ) -> // har ikke brig for at tjekke gennemsnit farve fordi max 255*2/2=255
    match (checkFigure f1) with
      | true ->
        (checkFigure f2)
      | false ->
        false

let rect0 = Rectangle((0,0),(0,0), (255,255,255))

let rect1 = Rectangle((50,50),(40,40), (255,255,255))

let rect2 = Rectangle((0,0),(50,50),(300,0,0))


let circ0 = Circle((50,50),-5,(255,255,255))
let circ1 = Circle((50,50),15,(-5,255,255))


printfn "%b" (checkFigure (figTest)=true)
printfn "%b" (checkFigure (rect0)=true)
printfn "%b" (checkFigure (rect1)=false)
printfn "%b" (checkFigure (rect2)=false)

printfn "%b" (checkFigure (circ0)=false)
printfn "%b" (checkFigure (circ1)=false)