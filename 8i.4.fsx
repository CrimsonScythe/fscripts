open ImgUtil

///  * move ()
/// <summary>
/// Moves figure according to vector
/// </summary>
/// <param name="vector"> the 2d directional vector to be applied </param>
/// <param name="fig"> the figure to be moved </param>
/// <returns>moved figure</returns>
/// <remarks>
/// Pattern matching is used to test the figure.
/// the vector is added to the x and y coordinates of the circle and rectangle.
/// The function is called recursively if fig has type Mix.
/// 
/// The function has been tested below using the makePicture() function.
/// </remarks>
let rec move (fig : figure) (vector:int*int) : figure= 
  match fig with
    | Circle ((cx, cy), r, col) ->
      Circle ((cx+fst(vector), cy+snd(vector)), r, col)
    | Rectangle ((x0, y0), (x1, y1), col) ->
      Rectangle ((x0+fst(vector), y0+snd(vector)), (x1+fst(vector), y1+snd(vector)), col)
    | Mix (f1, f2) ->
      let fig1 = move(f1) (vector)
      let fig2 = move(f2) (vector)
      Mix (fig1, fig2)

makePicture "moveTest.png" (move figTest (-20, 20)) 100 150