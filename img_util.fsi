module ImgUtil

type point = int * int // a point (x, y) in the plane
type colour = int * int * int // (red , green , blue ), 0..255
type figure =
| Circle of point * int * colour
// defined by center , radius , and colour
| Rectangle of point * point * colour
// defined by corners bottom -left , top -right , and colour
| Mix of figure * figure
// combine figures with mixed colour at overlap

val figTest : figure
val colourAt : int * int -> figure -> colour option
val makePicture : string -> figure -> int -> int -> unit
// val checkFigure : figure -> bool
val checkColour : colour -> bool
// val move : figure -> int * int -> figure
val boundingBox : figure -> point * point

// colors
type color = System.Drawing.Color
val red      : color
val blue     : color
val green    : color
val fromRgb  : int * int * int -> color

// bitmaps
type bitmap = System.Drawing.Bitmap
val mk       : int -> int -> bitmap
val setPixel : color -> int * int -> bitmap -> unit
// save a bitmap as a png file
val toPngFile : string -> bitmap -> unit

