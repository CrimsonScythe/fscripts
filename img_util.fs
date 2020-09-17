module ImgUtil

open System.Drawing
open System.Windows.Forms
open System

// finds colour of figure at point

type point = int * int // a point (x, y) in the plane
type colour = int * int * int // (red , green , blue ), 0..255
type figure =
| Circle of point * int * colour
// defined by center , radius , and colour
| Rectangle of point * point * colour
// defined by corners bottom -left , top -right , and colour
| Mix of figure * figure
// combine figures with mixed colour at overlap

let rec colourAt (x: int ,y: int ) figure : colour option =
  match figure with
  | Circle (( cx , cy ) , r , col ) ->
    if (x - cx ) * (x - cx) + (y - cy ) *(y - cy ) <= r*r
  // uses Pythagoras ' equation to determine
  // distance to center
    then Some col else None
  | Rectangle (( x0 , y0 ) , (x1 , y1 ) , col ) ->
    if x0 <= x && x <= x1 && y0 <= y && y <= y1
  // within corners
    then Some col else None
  | Mix (f1 , f2 ) ->
    match ( colourAt (x , y) f1 , colourAt (x ,y ) f2 ) with
      | ( None , c) -> c // no overlap
      | (c , None ) -> c // no overlap
      | ( Some (r1 ,g1 , b1 ) , Some (r2 ,g2 , b2 ) ) ->
    // average color
          Some (( r1 + r2 ) /2 , ( g1 + g2 ) /2 , ( b1 + b2 ) /2)



// colors
type color = System.Drawing.Color
let red : color = Color.Red
let blue : color = Color.Blue
let green : color = Color.Green

let fromRgb (r:int,g:int,b:int) : color =
  Color.FromArgb(255,r,g,b)
let format = Imaging.PixelFormat.Format24bppRgb

// bitmaps
type bitmap = System.Drawing.Bitmap
let mk w h : bitmap = new Bitmap (w,h,format)
let setPixel (c: color) (x,y) (bmp:bitmap) : unit = bmp.SetPixel (x,y,c)


// save a bitmap as a png file
let toPngFile (fname : string) (b: bitmap) : unit =
  b.Save(fname, Imaging.ImageFormat.Png) |> ignore


let figTest : figure =
  let red = (255,0,0)
  let blue = (0,0,255)
  let circle = Circle ((50,50), 45, red)
  let rect = Rectangle ((40,40),(90,110), blue)
  Mix (circle, rect)

let checkColour (col : colour)=
  match col with
    | (a,b,c) ->
      // (a<0 || a>255) || (b<0 || b>255) || (c<0 || c>255)
      if (a<0 || a>255) || (b<0 || b>255) || (c<0 || c>255) then
        false
      else
        true

// let rec checkFigure (fig : figure) : bool=
//  match fig with
//   | Circle (( cx , cy ) , r , col ) ->
//     match checkColour col with
//       | true ->
//         not (r<0)
//       | false ->
//         false

//   | Rectangle (( x0 , y0 ) , (x1 , y1 ) , col ) ->
//     match checkColour col with
//       | true ->
//          if x0<=x1 && y0<=y1 then
//             true
//           else
//             false
//         // (x0<=x1 && y0<=y1)
//       | false ->
//         false
   
//   | Mix (f1 , f2 ) -> // har ikke brig for at tjekke gennemsnit farve fordi max 255*2/2=255
//     match (checkFigure f1) with
//       | true ->
//         (checkFigure f2)
//       | false ->
//         false

// let rec move (fig : figure) (vector:int*int) : figure= 
//   match fig with
//     | Circle ((cx, cy), r, col) ->
//       Circle ((cx+fst(vector), cy+snd(vector)), r, col)
//     | Rectangle ((x0, y0), (x1, y1), col) ->
//       Rectangle ((x0+fst(vector), y0+snd(vector)), (x1+fst(vector), y1+snd(vector)), col)
//     | Mix (f1, f2) ->
//       let fig1 = move(f1) (vector)
//       let fig2 = move(f2) (vector)
//       Mix (fig1, fig2)

let makePicture (filnavn: string) (fig : figure) (b :int ) (h: int) : unit =

  let bmp = mk b h
  let mutable initX = 0
  let mutable initY = 0
  let mutable colorr = (128,128,128)

  for i = 1 to h-1 do
    for j=1 to b-1 do
      // colour <- (colourAt (j, i) fig)
      match colourAt (j, i) fig with
      | Some (r1, g1, b1)->
        colorr <- (r1,g1,b1)
      | _ ->
        colorr <- (128,128,128)

      setPixel (fromRgb (colorr)) (j ,i) bmp

  toPngFile filnavn bmp
let rec boundingBox (fig : figure) : point * point=
  let mutable mixx1=((0,0),(0,0))
  let mutable mixx2=((0,0),(0,0))
  match fig with
    | Circle ((x,y), r, col) ->
      (x-r, y-r), (x+r, y+r)
      // (x-r, 0), (x+r, y+r)
    |  Rectangle ((x0, y0), (x1, y1), col) ->
      (x0,y0), (x1,y1)
      // (x0,0), (x1,y1)
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
