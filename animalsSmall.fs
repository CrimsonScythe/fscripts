module animals

open System.IO
type symbol = char
type position = int * int
type neighbour = position * symbol

type ad = char
let mSymbol : symbol = 'm'
let wSymbol : symbol = 'w'
let eSymbol : symbol = ' '
let AA : ad = 'a'
let AD : ad = 'd'
let rnd = System.Random ()

type animal (symb : symbol, repLen : int) =
  let mutable _reproduction = rnd.Next(1,repLen)
  let mutable _pos : position option = None
  let _symbol : symbol = symb
  member this.symbol = _symbol
  member this.position
    with get () = _pos
    and set aPos = _pos <- aPos
  member this.reproduction = _reproduction
  member this.updateReproduction () =
    _reproduction <- _reproduction - 1
  member this.resetReproduction () =
    _reproduction <- repLen
  override this.ToString () =
    string this.symbol

type moose (repLen : int) =
  inherit animal (mSymbol, repLen) 
   member this.tick () : moose option =   
    if (this.reproduction < 1) then
        Some (this)
    else        
        None

type wolf (repLen : int, hungLen : int) =
  inherit animal (wSymbol, repLen)
  let mutable _hunger = hungLen
  member this.hunger = _hunger
  member this.updateHunger () =
    _hunger <- _hunger - 1
  member this.resetHunger () =
    _hunger <- hungLen
  member this.tick () : wolf option =
    if this.reproduction < 1 then
      Some (this)
    else
      None 

type board =
  {width : int;
   mutable moose : moose list;
   mutable wolves : wolf list;}

type environment (time : int,fileName : string, boardWidth : int, NMooses : int, mooseRepLen : int, NWolves : int, wolvesRepLen : int, wolvesHungLen : int, verbose : bool) =
  let _board : board = {
    width = boardWidth;
    moose = List.init NMooses (fun i -> moose(mooseRepLen));
    wolves = List.init NWolves (fun i -> wolf(wolvesRepLen, wolvesHungLen));
  }    
  let draw (b : board) : char [,] =
    let arr = Array2D.create<char> boardWidth boardWidth eSymbol
    for m in b.moose do
        Option.iter (fun p -> arr.[snd p, fst p] <- mSymbol) m.position
    for w in b.wolves do
        Option.iter (fun p -> arr.[snd p, fst p] <- wSymbol) w.position
    arr
  let anyEmptyField (b : board) : position =
    let arr = draw b
    let mutable i = rnd.Next b.width
    let mutable j = rnd.Next b.width
    while arr.[i,j] <> eSymbol do
      i <- rnd.Next b.width
      j <- rnd.Next b.width
    (i,j)

  /// * anyNeighbourField()
  /// <summary>
/// Finds and returns coordinates for an empty neighbour field. In the case of a moose, it finds an empty field,
/// in the case of wolves, it finds the coordinates of a moose to eat. If there is no moose then it finds an empty field.
/// </summary>
/// <param name="b"> The current board </param>
/// <param name="i">the x position of the current moose or wolf</param>
/// <param name="j">the y position of the current moose or wolf</param>
/// <param name="sym">variable of type symbol, indicating whether the animal is a moose or wolf</param>
/// <returns>Four options:
///   1.  Coordinates for the moose that will be eaten by the wolf.
///   2.  If there is no moose in the neighbour fields then it returns coordinates of an empty field the wolf will move to.
///   3.  In the case of the moose, it will return an empty field the moose will move to.
///   4.  If there is no empty field it returns the coordinates (-1,-1). </returns>
/// <remarks>
/// The code uses two empty lists, mArray and wolfArray to store all possible values for the fields.
/// So, if there is more than one field the moose can move to for example all possible values will be stored in mArray.
/// Then in the end of the code, the function will return a random index from the array.
/// This way the code ensures that the movement is always random.
/// </remarks>  
  let anyNeighbourField (b:board, i:int, j:int, sym:symbol) : position= 

    let arr = draw b
    let mutable minusi = i-1
    let mutable plusi = i+1
    let mutable minusj = j-1
    let mutable plusj = j+1

    let mutable mArray = []
    let mutable wolfArray = []
  
    for k = 0 to 2 do
      if ((minusi+k) >= 0) && ((minusi+k) < b.width) && (minusj >= 0) && (minusj < b.width) then
         
        match sym with
        |'w' -> 
              if ((arr.[(minusj), minusi+k]) = mSymbol) then              
                wolfArray <- (minusi+k, minusj) :: wolfArray
              else
                if ((arr.[(minusj), minusi+k]) = eSymbol) then                 
                  mArray <- (minusi+k, minusj) :: mArray
        |_ ->
            if ((arr.[(minusj), minusi+k]) = eSymbol) then             
              mArray <- (minusi+k, minusj) :: mArray

    for l = 0 to 2 do
      if ((minusi+l) >= 0) && ((minusi+l) < b.width) && (plusj >= 0) && (plusj < b.width) then
        match sym with
        | 'w' ->
        if ((arr.[plusj, minusi+l]) = mSymbol) then            
            wolfArray <- (minusi+l, plusj) :: wolfArray
        else
            if ((arr.[plusj, minusi+l]) = eSymbol) then          
              mArray <- (minusi+l, plusj) :: mArray
        |_ ->
          if ((arr.[plusj, minusi+l]) = eSymbol) then           
            mArray <- (minusi+l, plusj) :: mArray
    if (minusi >= 0) && (j >= 0) && (j < b.width) then
      match sym with
      |'w' ->
        if ((arr.[j, minusi]) = mSymbol) then         
          wolfArray<- (minusi, j) :: wolfArray
        else
          if ((arr.[j, minusi]) = eSymbol) then       
            mArray<- (minusi, j) :: mArray
      |_ ->
        if ((arr.[j, minusi]) = eSymbol) then        
          mArray<- (minusi, j) :: mArray    
    if (plusi < b.width) && (j >= 0) && (j < b.width) then
      match sym with
      |'w' ->
        if ((arr.[j, plusi]) = mSymbol) then         
          wolfArray<- (plusi, j) :: wolfArray
          elif 
            ((arr.[j, plusi]) = eSymbol) then            
              mArray<- (plusi, j) :: mArray
      |_ ->
        if ((arr.[j, plusi]) = eSymbol) then       
          mArray<- (plusi, j) :: mArray 

    if not(wolfArray.IsEmpty) then
      wolfArray.[rnd.Next(wolfArray.Length)]
    else
      if (mArray.IsEmpty) then
        (-1,-1)
      else  
        mArray.[rnd.Next(mArray.Length)]    

  do for m in _board.moose do
       m.position <- Some (anyEmptyField _board)
  do for w in _board.wolves do
       w.position <- Some (anyEmptyField _board)

  member this.size = boardWidth*boardWidth
  member this.count = _board.moose.Length + _board.wolves.Length
  member this.board = _board
  member this.tick () =
   let sw = File.CreateText fileName
   for ticks=1 to time do     
    printfn "%A" environment
    for m in _board.moose do
        m.updateReproduction()
    for w in _board.wolves do
        w.updateReproduction()
        w.updateHunger()
    let randomAnimal = rnd.Next (2)
    if randomAnimal = 0 && not(_board.wolves.IsEmpty) then      
      let mutable randomNumber = rnd.Next(_board.wolves.Length)     
      if _board.wolves.[randomNumber].hunger < 1 then
        if verbose then
          printfn "%A" "wolf died"
        // printfn "old wolf boardsss :%A" _board.wolves
        let mutable tempList = []
        for len in _board.wolves do
            if not(_board.wolves.[randomNumber] = len) then
                tempList <- len :: tempList
        printfn "temp list:%A" tempList
        _board.wolves <- []
        for elm in tempList do
            _board.wolves <- elm :: _board.wolves
        tempList <- []
        // printfn "wolf boardsss:%A" _board.wolves
      else if not(_board.moose.IsEmpty) then
        do match _board.wolves.[randomNumber].tick() with
            | Some parentWolf ->
              printfn "original position is: %A" parentWolf.position 
              let x = fst(parentWolf.position.Value)
              let y = snd(parentWolf.position.Value)
              let mutable coordinates = parentWolf.position.Value
              if not((anyNeighbourField (_board, x, y, 'm'))=(-1,-1)) then
                coordinates <- anyNeighbourField (_board, x, y, 'm')
                _board.wolves <- (new wolf (wolvesRepLen,wolvesHungLen)) :: _board.wolves 
                _board.wolves.Head.position <- Some coordinates
                parentWolf.resetReproduction()
                if verbose then
                  printfn "%A" "Wolf birth" 
              else
                if verbose then
                  printfn "%A" "no space"
            | None -> 
              let arr = draw _board
              let x = (fst(_board.wolves.[randomNumber].position.Value))
              let y = (snd(_board.wolves.[randomNumber].position.Value))
              let mutable coordinates = _board.wolves.[randomNumber].position.Value    
              printfn "old position wolf : %A" coordinates       
              coordinates <- anyNeighbourField (_board, x, y, 'w')
              let mutable tempList2 = []
              if arr.[snd(coordinates), fst(coordinates)] = mSymbol then
                if verbose then
                  printfn "%A" "moose died"

                _board.wolves.[randomNumber].resetHunger()
                               
                for o = 0 to _board.moose.Length-1 do
                    if (_board.moose.[o].position.Value)=coordinates then                      
                        for m in _board.moose do
                            if not(_board.moose.[o]=m) then
                                tempList2<-m :: tempList2
                _board.moose <- []
                for elem in tempList2 do
                    _board.moose <- elem :: _board.moose
                tempList2 <- []
                _board.wolves.[randomNumber].position <- Some coordinates
              else if not((anyNeighbourField (_board, x, y, 'm'))=(-1,-1)) then
                if verbose then
                  printfn "%A" "wolf moved"
                printfn "new position wolf is: %A" coordinates  
                _board.wolves.[randomNumber].position <- Some coordinates
              else
                if verbose then
                  printfn "%A" "no space"
                  
    else if not(_board.moose.IsEmpty) then     
      let mutable randomNumber = rnd.Next(_board.moose.Length)
      do match _board.moose.[randomNumber].tick() with
          | Some parentMoose ->
            printfn "original position is: %A" parentMoose.position 
            let x = fst(parentMoose.position.Value)
            let y = snd(parentMoose.position.Value)
            let mutable coordinates = parentMoose.position.Value
            
            if not((anyNeighbourField (_board, x, y, 'm'))=(-1,-1)) then
              coordinates <- anyNeighbourField (_board, x, y, 'm')            
              _board.moose <- (new moose (mooseRepLen)) :: _board.moose
              _board.moose.Head.position <- Some coordinates
              parentMoose.resetReproduction()
              if verbose then
                printfn "%A" "Moose birth"
            else
              if verbose then
                printfn "%A" "no space"

          | None->
            if verbose then
              printfn "%A" "moose moved"
            printfn "original position is: %A" _board.moose.[randomNumber].position.Value 
            
            let x = (fst(_board.moose.[randomNumber].position.Value))
            let y = (snd(_board.moose.[randomNumber].position.Value))
            let mutable coordinates = _board.moose.[randomNumber].position.Value
           
            if not((anyNeighbourField (_board, x, y, 'm')) = (-1,-1)) then 
              coordinates <- anyNeighbourField (_board, x, y, 'm')
              printfn "new position is:%A" coordinates  
              _board.moose.[randomNumber].position <- Some coordinates
            else
              printfn "%A" "no empty field"

    sw.WriteLine ("Time : " + ticks.ToString() + " " + "Mooses : " + _board.moose.Length.ToString() +  " " + "Wolves : " + _board.wolves.Length.ToString())
    sw.Flush()
    printfn "%A" this
   
  override this.ToString () =
    let arr = draw _board
    let mutable ret = "  "
    for j = 0 to _board.width-1 do
      ret <- ret + string (j % 10) + " "
    ret <- ret + "\n"
    for i = 0 to _board.width-1 do
      ret <- ret + string (i % 10) + " "
      for j = 0 to _board.width-1 do
        ret <- ret + string arr.[i,j] + " "
      ret <- ret + "\n"
    ret

///</...> 