open ImgUtil

///  * boundingBox ()
/// <summary>
/// Finds the top right and bottom left coordinates of an invisible 'box' that will contain the entire figure.
/// </summary>
/// <param name="fig"> the figure to be analysed </param>
/// <returns>top left and bottom right coordinates as a tuple (int * int)</returns>
/// <remarks>
/// If the figure is a Circle the radius is added to the x and y coordinates to get the 'box'.
/// If the figure is a rectangle the same coordinates are returned.
/// 
/// If the figure is of type Mix then we create four mutable variables upperX,upperY,lowerX,lowerY.
/// 
/// Four if blocks are used to test the X and Y coordinates for fig1 and fig2 (figures in Mix).
/// The final upperX and upperY are the smallest X and Y coordinates of fig1 and fgi2.
/// Similarly the lowerX and lowerY are the largest X and Y coordinates of fig1 and fig2.
/// This way we can be sure that the 'box' covers the whole figure.
/// 
/// The first if block tests the top left X coordinates.
/// The second if block tests the top left Y coordinates.
/// The third tests the bottom X and fourth bottom Y.
/// 
/// The variables are then returned as a tuple.
/// The function is called recursively in the if statements.
/// The function is tested below.
/// <code>
///      let mutable upperX=0
///      let mutable upperY=0
///      let mutable lowerX=0
///      let mutable lowerY=0

///      if fst(fst(boundingBox fig1)) < fst(fst(boundingBox fig2)) then
///        upperX <- (fst(fst(boundingBox fig1)))
///      else if fst(fst(boundingBox fig2)) < fst(fst(boundingBox fig1)) then
///        upperX <- (fst(fst(boundingBox fig2)))

///      if snd(fst(boundingBox fig1)) < snd(fst(boundingBox fig2)) then
///        upperY <- (snd(fst(boundingBox fig1)))
///      else if snd(fst(boundingBox fig2)) < snd(fst(boundingBox fig1)) then
///        upperY <- (snd(fst(boundingBox fig2)))

///      if fst(snd(boundingBox fig1)) > fst(snd(boundingBox fig2)) then
///        lowerX <- fst(snd(boundingBox fig1))
///      else if fst(snd(boundingBox fig2)) > fst(snd(boundingBox fig1)) then
///        lowerX <- fst(snd(boundingBox fig2))
      
///      if snd(snd(boundingBox fig1)) > snd(snd(boundingBox fig2)) then
///        lowerY <- snd(snd(boundingBox fig1))
///      else if snd(snd(boundingBox fig2)) > snd(snd(boundingBox fig1)) then
///        lowerY <- snd(snd(boundingBox fig2))
      
///      ((upperX,upperY),(lowerX,lowerY))
/// </code>
/// </remarks>
let rec boundingBox (fig : figure) : point * point=
  match fig with
    | Circle ((x,y), r, col) ->
      (x-r, y-r), (x+r, y+r) 
    |  Rectangle ((x0, y0), (x1, y1), col) ->
      (x0,y0), (x1,y1)
    | Mix (fig1, fig2) ->
     
      let mutable upperX=0
      let mutable upperY=0
      let mutable lowerX=0
      let mutable lowerY=0

      if fst(fst(boundingBox fig1)) < fst(fst(boundingBox fig2)) then
        upperX <- (fst(fst(boundingBox fig1)))
      else if fst(fst(boundingBox fig2)) < fst(fst(boundingBox fig1)) then
        upperX <- (fst(fst(boundingBox fig2)))

      if snd(fst(boundingBox fig1)) < snd(fst(boundingBox fig2)) then
        upperY <- (snd(fst(boundingBox fig1)))
      else if snd(fst(boundingBox fig2)) < snd(fst(boundingBox fig1)) then
        upperY <- (snd(fst(boundingBox fig2)))

      if fst(snd(boundingBox fig1)) > fst(snd(boundingBox fig2)) then
        lowerX <- fst(snd(boundingBox fig1))
      else if fst(snd(boundingBox fig2)) > fst(snd(boundingBox fig1)) then
        lowerX <- fst(snd(boundingBox fig2))
      
      if snd(snd(boundingBox fig1)) > snd(snd(boundingBox fig2)) then
        lowerY <- snd(snd(boundingBox fig1))
      else if snd(snd(boundingBox fig2)) > snd(snd(boundingBox fig1)) then
        lowerY <- snd(snd(boundingBox fig2))
      
      ((upperX,upperY),(lowerX,lowerY))

let col = (255,0,0)

let circles = Mix(Circle((50,50), 15, col),Circle((60,60), 15, col)) 

printfn "%b" ((boundingBox circles)=((35,35), (75,75)))
printfn "%b" ((boundingBox figTest)=((5,5), (95,110)))