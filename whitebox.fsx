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

  do for m in _board.moose do
       m.position <- Some (anyEmptyField _board)
  do for w in _board.wolves do
       w.position <- Some (anyEmptyField _board)

  // Unit: anyNeighbourField
  member this.anyNeighbourField (b:board, i:int, j:int, sym:symbol) : position= 

    // printfn "board this: %A" b
    
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
              if ((arr.[(minusj), minusi+k]) = mSymbol) then    (*WB: 1*)
                wolfArray <- (minusi+k, minusj) :: wolfArray
              else
                if ((arr.[(minusj), minusi+k]) = eSymbol) then     (*WB: 2*)            
                  mArray <- (minusi+k, minusj) :: mArray
        |_ ->
            if ((arr.[(minusj), minusi+k]) = eSymbol) then       (*WB: 3*)      
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

        
 

  member this.size = boardWidth*boardWidth
  member this.count = _board.moose.Length + _board.wolves.Length
  member this.board = _board
  //Unit: environment.tick()
  member this.tick () =
   let mutable finalText=""
  //  let sw = File.CreateText fileName
  //  let sw2 = File.CreateText "yay2.txt"
   let mutable countMoose=0
   let mutable countWolf=0
   for ticks=1 to time do     
    // printfn "%A" environment
    for m in _board.moose do
        m.updateReproduction()
    for w in _board.wolves do
        w.updateReproduction()
        w.updateHunger()
    let randomAnimal = rnd.Next (2)
    if randomAnimal = 0 && not(_board.wolves.IsEmpty) then      
      let mutable randomNumber = rnd.Next(_board.wolves.Length)     
      if _board.wolves.[randomNumber].hunger < 1 then       (*WB: 1*)
        
        // printfn "old wolf boardsss :%A" _board.wolves
        let mutable tempList = []
        for len in _board.wolves do
            if not(_board.wolves.[randomNumber] = len) then     
                tempList <- len :: tempList
        // printfn "temp list:%A" tempList
        _board.wolves <- []
        for elm in tempList do
            _board.wolves <- elm :: _board.wolves
        tempList <- []
        if verbose then
          printfn "%A" "wolf died"
        finalText <- "wolf died"
        
        // printfn "wolf boardsss:%A" _board.wolves
      else if not(_board.moose.IsEmpty) then                    
        do match _board.wolves.[randomNumber].tick() with
            | Some parentWolf ->  
              // printfn "original position is: %A" parentWolf.position 
              let x = fst(parentWolf.position.Value)
              let y = snd(parentWolf.position.Value)
              let mutable coordinates = parentWolf.position.Value
              if not((this.anyNeighbourField (_board, x, y, 'm'))=(-1,-1)) then      (*WB: 2*)
                coordinates <- this.anyNeighbourField (_board, x, y, 'm')
                _board.wolves <- (new wolf (wolvesRepLen,wolvesHungLen)) :: _board.wolves 
                _board.wolves.Head.position <- Some coordinates
                parentWolf.resetReproduction()
                if verbose then
                  printfn "%A" "Wolf birth" 
                finalText <- "wolf birth" 
              else                                                                   (*WB: 3*)
                if verbose then
                  printfn "%A" "no space"
                finalText <- "no space"
            | None -> 
              let arr = draw _board
              let x = (fst(_board.wolves.[randomNumber].position.Value))
              let y = (snd(_board.wolves.[randomNumber].position.Value))
              let mutable coordinates = _board.wolves.[randomNumber].position.Value    
              // printfn "old position wolf : %A" coordinates       
              coordinates <- this.anyNeighbourField (_board, x, y, 'w')
              // printfn "old board moose%A" _board.moose
              let mutable tempList2 = []
              if arr.[snd(coordinates), fst(coordinates)] = mSymbol then        (*WB: 4*)
                if verbose then
                  printfn "%A" "moose died"

                _board.wolves.[randomNumber].resetHunger()
                               
                for o = 0 to _board.moose.Length-1 do
                    if (_board.moose.[o].position.Value)=coordinates then                      
                        for m in _board.moose do
                            if not(_board.moose.[o]=m) then
                                tempList2<-m :: tempList2
                                // printfn "templist2: %A" tempList2
                _board.moose <- []
                for elem in tempList2 do
                    _board.moose <- elem :: _board.moose
                tempList2 <- []
                  // printfn "moosr board : %A" _board.moose 
                _board.wolves.[randomNumber].position <- Some coordinates
                finalText <- "moose died"
              else if not((this.anyNeighbourField (_board, x, y, 'm'))=(-1,-1)) then     (*WB: 5*)
                if verbose then
                  printfn "%A" "wolf moved"
                // printfn "new position wolf is: %A" coordinates  
                _board.wolves.[randomNumber].position <- Some coordinates
                finalText <- "wolf moved"
              else
                if verbose then
                  printfn "%A" "no space"
                finalText <- "no space"
                  
    else if not(_board.moose.IsEmpty) then     
      let mutable randomNumber = rnd.Next(_board.moose.Length)
      do match _board.moose.[randomNumber].tick() with
          | Some parentMoose ->
            // printfn "original position is: %A" parentMoose.position 
            let x = fst(parentMoose.position.Value)
            let y = snd(parentMoose.position.Value)
            let mutable coordinates = parentMoose.position.Value
            
            if not((this.anyNeighbourField (_board, x, y, 'm'))=(-1,-1)) then        (*WB: 6*)
              coordinates <- this.anyNeighbourField (_board, x, y, 'm')            
              _board.moose <- (new moose (mooseRepLen)) :: _board.moose
              _board.moose.Head.position <- Some coordinates
              parentMoose.resetReproduction()
              if verbose then
                printfn "%A" "Moose birth"
              finalText <- "moose birth"
            else
              if verbose then
                printfn "%A" "no space"
              finalText <- "no space"

          | None->
            if verbose then
              printfn "%A" "moose moved"
            // printfn "original position is: %A" _board.moose.[randomNumber].position.Value 
            
            let x = (fst(_board.moose.[randomNumber].position.Value))
            let y = (snd(_board.moose.[randomNumber].position.Value))
            let mutable coordinates = _board.moose.[randomNumber].position.Value
           
            if not((this.anyNeighbourField (_board, x, y, 'm')) = (-1,-1)) then      (*WB: 7*)
              coordinates <- this.anyNeighbourField (_board, x, y, 'm')
              // printfn "new position is:%A" coordinates  
              _board.moose.[randomNumber].position <- Some coordinates
              finalText <- "moose moved"
            else
              // printfn "%A" "no empty field"
              finalText <- "no space"
   finalText
    // sw.WriteLine ("Time : " + ticks.ToString() + " " + "Mooses : " + _board.moose.Length.ToString() +  " " + "Wolves : " + _board.wolves.Length.ToString())
    // sw.WriteLine(_board.moose.Length.ToString())
    // sw.Flush()
    // sw2.WriteLine(_board.wolves.Length.ToString())
    // sw2.Flush()
    // countMoose <- 0
    // countWolf <- 0
    // printfn "%A" this
    // printfn "moose list = %A" _board.moose
    // printfn "wolf list = %A" _board.wolves 
   
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

printfn "\n White-box testing of 10g"
printfn "   Unit: anyNeighbourField" 

// printfn "       Branch: 1 - %b \n" ((play [|3;3;3;3;3;3;6;0;0;0;0;0;0;8|] player1) = (play [|3;3;3;3;3;3;6;0;0;0;0;0;0;8|] player1))
// printfn "       Branch: 2 - %b \n" ((play [|0;0;0;0;0;0;8;3;3;3;3;3;3;6|] player2) = (play [|0;0;0;0;0;0;8;3;3;3;3;3;3;6|] player2))


let brd = environment(1,"test.txt", 5, 24, 10, 1, 30, 30, false)

printfn "       Branch: 1 - %b \n" (not(brd.anyNeighbourField(brd.board, fst(brd.board.wolves.[0].position.Value), snd(brd.board.wolves.[0].position.Value) , 'w')=(fst(brd.board.wolves.[0].position.Value),snd(brd.board.wolves.[0].position.Value))))
let brd2 = environment(1,"test.txt", 5, 0, 10, 1, 30, 30, false)

printfn "       Branch: 2 - %b \n" (not(brd2.anyNeighbourField(brd2.board, fst(brd2.board.wolves.[0].position.Value), snd(brd2.board.wolves.[0].position.Value) , 'w')=(fst(brd2.board.wolves.[0].position.Value),snd(brd2.board.wolves.[0].position.Value))))
let brd3 = environment(1,"test.txt", 5, 1, 10, 0, 30, 30, false)
printfn "       Branch: 3 - %b \n" (not(brd3.anyNeighbourField(brd3.board, fst(brd3.board.moose.[0].position.Value), snd(brd3.board.moose.[0].position.Value) , 'm')=(fst(brd3.board.moose.[0].position.Value),snd(brd3.board.moose.[0].position.Value))))

// let brd4 = environment(1,"test.txt", 2, 4, 10, 0, 30, 30, false)
// printfn "       Branch: 4 - %b \n" (brd4.anyNeighbourField(brd4.board, fst(brd4.board.moose.[0].position.Value), snd(brd4.board.moose.[0].position.Value) , 'm')=(-1,-1))

printfn "   Unit: environment.tick()"

let brd5 = environment(1,"test.txt", 2, 0, 10, 1, 30, 0, false)
let mutable re = brd5.tick()
while not(re="wolf died") do
  re <- brd5.tick()

printfn "     Branch: 1 - %b \n" (re="wolf died")

let brd6 = environment(1,"test.txt", 9, 1, 1000, 1, 1, 1000, false)
let mutable ree = brd6.tick()
while not(ree="wolf birth") do
  ree <- brd6.tick()

printfn "     Branch: 2 - %b \n" (ree="wolf birth")


let brd7 = environment(1,"test.txt", 2, 10, 1, 10, 1, 1000, false)
let mutable reee = brd7.tick()
while not(reee="no space") do
  reee <- brd7.tick()

printfn "     Branch: 3 - %b \n" (reee="no space")

let brd8 = environment(1,"test.txt", 6, 1, 1000, 10, 1000, 1000, false)
let mutable reeee = brd8.tick()
while not(reeee="moose died") do
  reeee <- brd8.tick()

printfn "     Branch: 4 - %b \n" (reeee="moose died")


let brd9 = environment(1,"test.txt", 9, 1, 1000, 10, 1000, 1000, false)
let mutable rrr = brd9.tick()
while not(rrr="wolf moved") do
  rrr <- brd9.tick()

printfn "     Branch: 5 - %b \n" (rrr="wolf moved")

let brd10 = environment(1,"test.txt", 9, 5, 1, 1, 1000, 1000, false)
let mutable kk = brd10.tick()
while not(kk="moose birth") do
  kk <- brd10.tick()

printfn "     Branch: 6 - %b \n" (kk="moose birth")

let brd11 = environment(1,"test.txt", 9, 5, 1000, 1, 1000, 1000, false)
let mutable kkk = brd11.tick()
while not(kkk="moose moved") do
  kkk <- brd11.tick()

printfn "     Branch: 7 - %b \n" (kkk="moose moved")