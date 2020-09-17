open System.Drawing
open System.Windows.Forms
open System
open System.Drawing.Drawing2D
open System.Text.RegularExpressions

///* DrawClass ()
/// <summary>
/// This is the main (and only) class responsible for drawing the clock and contains all functions for the clock.
/// </summary>
/// <returns>no return value</returns>
/// <remarks>
/// </remarks>

type DrawClass ()=
    
    let _win = new System.Windows.Forms.Form()
    let mutable _timer=0    
    let mutable _first = false
    let mutable _rotateAngle = 0.0f
    let mutable _rotateAngle2 = 0.0f
    let mutable _rotateAngle3 = 0.0f
    let mutable _color = Color.Black
    let mutable _internalTime=0
    let mutable _internalTime2=0
    
    let mutable _line1 = None
    let mutable _line2 = None
    let mutable _line3 = None

    let mutable _hours = 12
    let mutable _mins = 0
    let mutable _seconds = -1

    let mutable _currentTime = None
    let mutable _currentDate = None    

    let mutable _clockFace = None


    member this.TimerValue
        with get()=
            _timer
        and set aTimer=
            _timer <- aTimer
    member this.Window = _win

    ///* this.SetUpWindow ()
/// <summary>
/// Sets the window size.
/// </summary>
/// <returns>no return value</returns>
/// <remarks>
/// </remarks>
    member this.SetUpWindow ()  :   unit=
        this.Window.Size <- new Size (400, 400)

///* this.PaintCanvas ()
/// <summary>
/// Draws the circle for the clock face.
/// </summary>
/// <params>
/// Default parameter for this callback method. 
/// It is used to access Graphics and draw the circle.
/// </params>
/// <returns>no return value</returns>
/// <remarks>
/// </remarks>
    member this.PaintCanvas (e : PaintEventArgs):unit=
        let rect = RectangleF (new PointF(0.0f,0.0f), new SizeF(300.0f, 300.0f))
        e.Graphics.DrawEllipse (new Pen (Color.Black), rect)

///* this.TimerEventProcessor ()
/// <summary>
/// Callback function for the tick event. 
/// Updates the window UI every 1000 ms.
/// </summary>
/// <returns>no return value</returns>
/// <remarks>
/// </remarks>
    member this.TimerEventProcessor (e : EventArgs) : unit =
        this.Window.Invalidate()
 
 
    member this.line1
        with get()=
            _line1
        and set aline=
            _line1 <- aline

    member this.line2
        with get()=
            _line2
        and set aline=
            _line2 <- aline            

    member this.line3
        with get()=
            _line3
        and set aline=
            _line3 <- aline    

    member this.currentTime
        with get()=
            _currentTime
        and set aline=
            _currentTime <- aline  

    member this.currentDate
        with get()=
            _currentDate
        and set aline=
            _currentDate <- aline      

    member this.clockface
        with get()=
            _clockFace
        and set x=
            _clockFace <- x                                        

    member this.InternalTime
        with get()=
            _internalTime
        and set aTime=
            _internalTime <- aTime

    member this.InternalTime2
        with get()=
            _internalTime2
        and set aTime=
            _internalTime2 <- aTime

///* this.SetUpLines ()
/// <summary>
/// Initializes the three clock hands.
/// </summary>
/// <returns>no return value</returns>
/// <remarks>
/// </remarks>
    member this.SetUpLines ()=
        this.line1 <- Some (this.Window.CreateGraphics())
        this.line2 <- Some (this.Window.CreateGraphics())
        this.line3 <- Some (this.Window.CreateGraphics())
 
 ///* DrawClass ()
/// <summary>
/// Initializes labels for the time and date.
/// </summary>
/// <returns>no return value</returns>
/// <remarks>

/// </remarks>
    member this.SetUpLabels ()=
        this.currentTime <- Some (new Label())
        this.currentDate <- Some (new Label())
        this.clockface <- Some (new Label())

    member this.ColorIs
        with get()=
            _color
        and set aColor=
            _color <- aColor

    member this.First 
        with get() =
            _first
        and set aFirst =
            _first <- aFirst
    
    member this.RotateAngle 
        with get() =
             _rotateAngle
        and set aAngle =
             _rotateAngle <- aAngle 
    
    member this.RotateAngle2 
        with get() =
             _rotateAngle2
        and set x =
             _rotateAngle2 <- x 

    member this.RotateAngle3 
        with get() =
             _rotateAngle3
        and set x =
             _rotateAngle3 <- x              


///* this.Paint ()
/// <summary>
/// Main function that is reponsible for updating the time on the clock and rotating the second, minute and hour hands.
/// </summary>
/// <params>
/// Default parameter for the callback method.
/// </params>
/// <returns>no return value</returns>
/// <code>
///     let mm = new Matrix()
///     this.RotateAngle2 <- this.RotateAngle2 + 6.0f
///     mm.RotateAt(this.RotateAngle2, new PointF(150.0f, 150.0f))
///     this.line2.Value.Transform <- mm
/// </code>
/// The code uses the RotateAt function on a matrix to define the rotation.
/// This matrix is then applied to to the Transform value of the respective line.
/// <remarks>
/// The variables InternalTime and InternalTime2 are used to keep track of the elapsed time since the clock is started.
/// They are used to check if the minute and hour hands need to be rotated. Naturally, the second hand is updated every second.
/// Every time the threshold is reached (59 seconds for the minute hand and 3600 for the hour hand), the variables are reset to 0. 

/// </remarks>     
    member this.Paint (e : PaintEventArgs) : unit=
        this.InternalTime2 <- this.InternalTime2 + 1

        if (this.First) then  

            this.line2.Value.DrawLine (new Pen (Color.Green), new PointF(150.0f,150.0f), new PointF(150.0f,0.0f))
            this.line3.Value.DrawLine (new Pen (Color.Orange), new PointF(150.0f,150.0f), new PointF(150.0f,50.0f))
            this.Window.Controls.Add this.currentTime.Value
            this.currentTime.Value.Location <- new Point (100,200)
            this.currentTime.Value.BackColor <- Color.Orange        

            let regex = Regex.Split(System.DateTime.Now.ToLongTimeString(), ":")
          
            this.RotateAngle <- 6.0f * (float32(regex.[2]))
            let matrix = new Matrix()
            matrix.RotateAt(this.RotateAngle, new PointF(150.0f,150.0f))
            this.line1.Value.Transform <- matrix

            this.RotateAngle2 <- 6.0f * (float32(regex.[1]))
            let matrix2 = new Matrix()
            matrix2.RotateAt(this.RotateAngle2, new PointF(150.0f, 150.0f))
            this.line2.Value.Transform <- matrix2

            this.RotateAngle3 <- 6.0f * (float32(regex.[0])*5.0f)
            this.RotateAngle3 <- this.RotateAngle3 + ((float32(regex.[1])*60.0f*30.0f)/3600.0f)         
            let matrix3 = new Matrix()
            matrix3.RotateAt(this.RotateAngle3, new PointF(150.0f, 150.0f))
            this.line3.Value.Transform <- matrix3

            this.currentTime.Value.Text <- System.DateTime.Now.ToString()

          
        this.line1.Value.DrawLine (new Pen (Color.Red), new PointF(150.0f,150.0f), new PointF(150.0f,0.0f))
        this.line2.Value.DrawLine (new Pen (Color.Green), new PointF(150.0f,150.0f), new PointF(150.0f,0.0f))
        this.line3.Value.DrawLine (new Pen (Color.Orange), new PointF(150.0f,150.0f), new PointF(150.0f,0.0f))


        this.InternalTime <- this.InternalTime + 1

        this.First <- true      


        this.currentTime.Value.Text <- System.DateTime.Now.ToString()
        
///* this.SetUpTimer ()
/// <summary>
/// This function is reponsible for setting up the Main timer, which calls the TimerEventProcessor function every time it fires.
/// </summary>
/// <params>
/// instance of the DrawClass
/// </params>
/// <returns>no return value</returns>
/// <remarks>
/// The DrawClass instance is used to access the TimerEventProcessor function.
/// </remarks>     
    member this.SetUpTimer (instance : DrawClass)=

        let timer = new System.Windows.Forms.Timer()
        timer.Enabled
        timer.Interval <- this.TimerValue
        timer.Tick.Add (instance.TimerEventProcessor)
        timer.Start()


let newInstance = new DrawClass()
newInstance.SetUpWindow()
newInstance.Window.Paint.Add newInstance.PaintCanvas

newInstance.Window.Paint.Add newInstance.Paint
newInstance.TimerValue <- 1000
newInstance.SetUpTimer(newInstance)
newInstance.SetUpLines()
newInstance.SetUpLabels()



System.Windows.Forms.Application.Run newInstance.Window