open ImgUtil

/// <summary>
/// Uses inputs: fileName, figure, breadth and height to make a .png picture file containg the bitmap.
/// </summary>
/// <param name="filnavn"> File name of the .png file </param>
/// <param name="fig"> the figure to be drawn </param>
/// <param name="b"> the breadth of the final picture</param>
/// <param name="h">the heigth of the final picture</param>

/// <returns>saves .png picture file in project folder</returns>
/// <remarks>
/// The code uses the following functions from the ImgUtil library .fsi and .fs files:
///     -mk makes bitmap of breadth b and height h
///     -colourAt finds the colour in the figure to draw, if it returns None then colour is set to gray
///     -setPixel draws the figure in the bitmap canvas 
///     -toPngFile converts bitmap to .png file 
/// <code>
/// for i = 1 to h-1 do
///     for j=1 to b-1 do
///         match colourAt (j, i) fig with
///             | Some (r1, g1, b1)->
///                 colorr <- (r1,g1,b1)
///             | _ ->
///                 colorr <- (128,128,128)

///             setPixel (fromRgb (colorr)) (j ,i) bmp
/// </code>
/// The inner for loop runs through each pixel horizontally and colours each pixel using SetPixel(). 
/// The outer for loop chnages the vertical pixel each time the horizontal pixels have been coloured. 
/// </remarks>

let makePicture (filnavn: string) (fig : figure) (b :int ) (h: int) : unit =

  let bmp = mk b h
  let mutable colorr = (128,128,128)

  for i = 1 to h-1 do
    for j=1 to b-1 do
      match colourAt (j, i) fig with
      | Some (r1, g1, b1)->
        colorr <- (r1,g1,b1)
      | _ ->
        colorr <- (128,128,128)

      setPixel (fromRgb (colorr)) (j ,i) bmp

  toPngFile filnavn bmp