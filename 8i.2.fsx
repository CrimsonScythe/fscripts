open ImgUtil

/// <summary>
/// Testing the function makePicture
/// </summary>

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

let circle = Circle((50,50), 5, (255,255,0))
let rect = Rectangle((0,0), (50,50), (255,0,255))

makePicture ("figTest.png") figTest 100 150
makePicture ("circle.png") circle 100 100
makePicture ("rect.png") rect 100 100