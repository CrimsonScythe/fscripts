open ImgUtil    

/// <summary>
/// Creates figure of type Mix containing one circle and one rectangle
/// </summary>
/// <returns>figure of type Mix</returns>
/// <remarks>
/// The code uses Circle, Rectangle and Mix of the type: figure from the .fsi and .fs library files.
/// </remarks>
let figTest : figure =
  let red = (255,0,0)
  let blue = (0,0,255)
  let circle = Circle ((50,50), 45, red)
  let rect = Rectangle ((40,40),(90,110), blue)
  Mix (circle, rect)