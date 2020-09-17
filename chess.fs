module Chess (*//§\label{chessHeader}§*)
open System  
open System.IO
open System.Text
type Color = White | Black
type PlayerType = P1 | P2
type Position = int * int(*//§\label{chessTypeEnd}§*)
/// An abstract chess piece §\label{chessPieceBegin}§
/// TODO: king implementation
[<AbstractClass>]
type chessPiece(color : Color) =
  let mutable _position : Position option = None
  abstract member nameOfType : string // "king", "rook", ...
  member this.color = color // White, Black
  member this.position // E.g., (0,0), (3,4), etc.
    with get() = _position
    and set(pos) = _position <- pos
  override this.ToString () = // E.g. "K" for white king
    match color with
      White -> (string this.nameOfType.[0]).ToUpper ()
      | Black -> (string this.nameOfType.[0]).ToLower ()
  /// A list of runs, which is a list of relative movements, e.g.,
  /// [[(1,0); (2,0);...]; [(-1,0); (-2,0)]...]. Runs must be
  /// ordered such that the first in a list is closest to the piece
  /// at hand.      
  abstract member candiateRelativeMoves : Position list list
  /// Available moves and neighbours ([(1,0); (2,0);...], [p1; p2])
  member this.availableMoves (board : Board) : (Position list * chessPiece list) =
    
    board.getVacantNNeighbours this board (*//§\label{chessPieceEnd}§*)
/// A board §\label{chessBoardBegin}§
and Board () =
     
  let _array = Collections.Array2D.create<chessPiece option> 8 8 None
  /// Wrap a position as option type
  let validPositionWrap (pos : Position) : Position option =
    let (rank, file) = pos // square coordinate
    if rank < 0 || rank > 7 || file < 0 || file > 7 
    then None
    else Some (rank, file)
  /// Convert relative coordinates to absolute and remove out
  /// of board coordinates.
  let relativeToAbsolute (pos : Position) (lst : Position list) : Position list =
    let addPair (a : int, b : int) (c : int, d : int) : Position = 
      (a+c,b+d)
    // Add origin and delta positions
    List.map (addPair pos) lst
    // Choose absolute positions that are on the board
    |> List.choose validPositionWrap
  /// Board is indexed using .[,] notation
  member this.Item
    with get(a : int, b : int) = _array.[a, b]
    and set(a : int, b : int) (p : chessPiece option) = 
      if p.IsSome then p.Value.position <- Some (a,b)  (*//§\label{chessItemSet}§*)
      _array.[a, b] <- p
  /// Produce string of board for, e.g., the printfn function.
  override this.ToString() =
    let rec boardStr (i : int) (j : int) : string =
      match (i,j) with 
        (8,0) -> ""
        | _ ->
          let stripOption (p : chessPiece option) : string = 
            match p with
              None -> ""
              | Some p -> p.ToString()
          // print top to bottom row
          let pieceStr = stripOption _array.[7-i,j]
          //let pieceStr = sprintf "(%d, %d)" i j
          let lineSep = " " + String.replicate (8*4-1) "-"
          match (i,j) with 
          (0,0) -> 
            let str = sprintf "%s\n| %1s " lineSep pieceStr
            str + boardStr 0 1
          | (i,7) -> 
            let str = sprintf "| %1s |\n%s\n" pieceStr lineSep
            str + boardStr (i+1) 0 
          | (i,j) -> 
            let str = sprintf "| %1s " pieceStr
            str + boardStr i (j+1)
    boardStr 0 0
  /// Move piece by specifying source and target coordinates
  member this.move (source : Position) (target : Position) : unit =
    this.[fst target, snd target] <- this.[fst source, snd source]
    this.[fst source, snd source] <- None
  /// Find the tuple of empty squares and first neighbour if any.
  member this.getVacantNOccupied (run : Position list) : (Position list * (chessPiece option)) =
    try
      // Find index of first non-vacant square of a run
      let idx = List.findIndex (fun (i, j) -> this.[i,j].IsSome) run
      let (i,j) = run.[idx]
      let piece = this.[i, j] // The first non-vacant neighbour
      if idx = 0
      then ([], piece)
      else (run.[..(idx-1)], piece)
    with
      _ -> (run, None) // outside the board
  /// find the list of all empty squares and list of neighbours
  member this.getVacantNNeighbours (piece : chessPiece) (board : Board) : (Position list * chessPiece list)  =
    match piece.position with
      None -> 
        ([],[])
      | Some p ->
        let convertNWrap = 
          (relativeToAbsolute p) >> this.getVacantNOccupied
        let vacantPieceLists = List.map convertNWrap piece.candiateRelativeMoves
        // Extract and merge lists of vacant squares
        match piece.ToString() with
          "K" ->
            let mutable rPos = (0,0)
            //find r position
            for i = 0 to 7 do
              for j = 0 to 7 do
                match (board.Item(i, j)) with
                  Some k -> 
                    if (board.Item(i, j)).Value.ToString()="r" then
                      rPos <- (i, j)
                  | None -> rPos <- (-1,-1)
            // if (brd.Item(i, j)).Value.ToString() = "k" then
            if rPos=(-1,-1) then
                let vacant = List.collect fst vacantPieceLists
                let opponent = 
                  vacantPieceLists
                  |> List.choose snd 
                (vacant, opponent)(*//§\label{chessBoardEnd}§*) 
            else  
              let vacant = List.collect fst vacantPieceLists
              let mutable vacant2 = []
              let originalPos = piece.position.Value

              let mutable interPos = originalPos
              for i = 0 to vacant.Length-1 do
                board.move interPos vacant.[i]
                interPos <- vacant.[i]
                if ((snd(board.Item(fst(rPos),snd(rPos)).Value.availableMoves board))=[]) then
                  vacant2 <- (vacant.[i]) :: vacant2
            //return piece to original position    
              board.move vacant.[vacant.Length-1] originalPos
            
        // Extract and merge lists of first obstruction pieces and filter out own pieces
              let opponent = 
                vacantPieceLists
                |> List.choose snd 
              (vacant2, opponent)(*//§\label{chessBoardEnd}§*) 
          | "k" ->

            let mutable rPos = (0,0)
            //find R position
            for i = 0 to 7 do
              for j = 0 to 7 do
                match (board.Item(i, j)) with
                  Some k -> 
                    if (board.Item(i, j)).Value.ToString()="R" then
                      rPos <- (i, j)
                  | None -> rPos <- (-1,-1)
            if rPos=(-1,-1) then
                let vacant = List.collect fst vacantPieceLists
                let opponent = 
                  vacantPieceLists
                  |> List.choose snd 
                (vacant, opponent)(*//§\label{chessBoardEnd}§*) 
            else

              let vacant = List.collect fst vacantPieceLists
              let mutable vacant2 = []
              let originalPos = piece.position.Value
              let mutable interPos = originalPos
              for i = 0 to vacant.Length-1 do
                board.move interPos vacant.[i]
                interPos <- vacant.[i]
                if ((snd(board.Item(1,1).Value.availableMoves board))=[]) then
                  vacant2 <- (vacant.[i]) :: vacant2
            //return piece to original position    
              board.move vacant.[vacant.Length-1] originalPos
          
        // Extract and merge lists of first obstruction pieces and filter out own pieces
              let opponent = 
                vacantPieceLists
                |> List.choose snd 
              (vacant2, opponent)(*//§\label{chessBoardEnd}§*) 
          |_ ->

            let vacant = List.collect fst vacantPieceLists
        // Extract and merge lists of first obstruction pieces and filter out own pieces
            let opponent = 
              vacantPieceLists
              |> List.choose snd 
            (vacant, opponent)(*//§\label{chessBoardEnd}§*) 
            

type Player(plr : PlayerType) = 
  
  let _player = plr

  member this.player = _player


type Human(playerIs : PlayerType)=
  inherit Player(playerIs) 

  member this.nextMove (brd : Board) = 

    let mutable _p1List_r = []
    let mutable _p1List_k = []
    let mutable _p2List_R = []
    let mutable _p2List_K = []

    let mutable orgPosP1_k=""
    let mutable orgPosP2_K=""
    let mutable orgPosP1_r=""
    let mutable orgPosP2_R=""

    for i = 0 to 7 do
      for j = 0 to 7 do
        match (brd.Item(i, j)) with
        Some k ->
          // printfn "%A" "IT WORKS"
          // printfn "%A" ((brd.Item(i, j).Value).ToString())
          if (brd.Item(i, j)).Value.ToString() = "k" then
            _p1List_k <- (brd.Item(i, j).Value.availableMoves brd) :: _p1List_k 
            match snd(i, j) with
              0 -> orgPosP1_k <- "a"+i.ToString()
              | 1 -> orgPosP1_k <- "b"+i.ToString()
              | 2 ->orgPosP1_k <- "c"+i.ToString()
              | 3 ->orgPosP1_k <- "d"+i.ToString()
              | 4 ->orgPosP1_k <- "e"+i.ToString()
              | 5 ->orgPosP1_k <- "f"+i.ToString()
              | 6 ->orgPosP1_k <- "g"+i.ToString()
              | 7 ->orgPosP1_k <- "h"+i.ToString()
              | _ -> () 
          elif (brd.Item(i, j)).Value.ToString() = "r" then
            _p1List_r <- (brd.Item(i, j).Value.availableMoves brd) :: _p1List_r 
            match snd(i, j) with
              0 -> orgPosP1_r <- "a"+i.ToString()
              | 1 -> orgPosP1_r <- "b"+i.ToString()
              | 2 ->orgPosP1_r <- "c"+i.ToString()
              | 3 ->orgPosP1_r <- "d"+i.ToString()
              | 4 ->orgPosP1_r <- "e"+i.ToString()
              | 5 ->orgPosP1_r <- "f"+i.ToString()
              | 6 ->orgPosP1_r <- "g"+i.ToString()
              | 7 ->orgPosP1_r <- "h"+i.ToString()
              | _ -> ()
          elif (brd.Item(i, j)).Value.ToString() = "K" then
            _p2List_K <- (brd.Item(i, j).Value.availableMoves brd) :: _p2List_K 
            match snd(i, j) with
              0 -> orgPosP2_K <- "a"+i.ToString()
              | 1 -> orgPosP2_K <- "b"+i.ToString()
              | 2 ->orgPosP2_K <- "c"+i.ToString()
              | 3 ->orgPosP2_K <- "d"+i.ToString()
              | 4 ->orgPosP2_K <- "e"+i.ToString()
              | 5 ->orgPosP2_K <- "f"+i.ToString()
              | 6 ->orgPosP2_K <- "g"+i.ToString()
              | 7 ->orgPosP2_K <- "h"+i.ToString()
              | _ -> ()
          elif (brd.Item(i, j)).Value.ToString() = "R" then
            _p2List_R <- (brd.Item(i, j).Value.availableMoves brd) :: _p2List_R 
            match snd(i,j) with
              0 -> orgPosP2_R <- "a"+i.ToString()
              | 1 -> orgPosP2_R <- "b"+i.ToString()
              | 2 ->orgPosP2_R <- "c"+i.ToString()
              | 3 ->orgPosP2_R <- "d"+i.ToString()
              | 4 ->orgPosP2_R <- "e"+i.ToString()
              | 5 ->orgPosP2_R <- "f"+i.ToString()
              | 6 ->orgPosP2_R <- "g"+i.ToString()
              | 7 ->orgPosP2_R <- "h"+i.ToString()
              | _ -> ()  
        | _ -> () 
    // printfn "%A" (_p1List_k)
    // printfn "%A" (_p2List_K)
    // printfn "%A" (_p1List_r)
    // printfn "%A" (_p2List_R)

    let mutable movesListp1_k = []
    let mutable movesListp1_r = []
    let mutable movesListp2_K = []
    let mutable movesListp2_R = []

    if not((_p1List_k).IsEmpty) then
      for i = 0 to (fst((_p1List_k).Head)).Length-1 do
        match (snd(((fst((_p1List_k).Head)).[i]))) with
          0 -> movesListp1_k <- (orgPosP1_k,"a"+(fst(((fst((_p1List_k).Head)).[i]))).ToString()) :: movesListp1_k
          | 1 -> movesListp1_k <- (orgPosP1_k,"b"+(fst(((fst((_p1List_k).Head)).[i])).ToString())) :: movesListp1_k
          | 2 -> movesListp1_k <- (orgPosP1_k,"c"+(fst(((fst((_p1List_k).Head)).[i]))).ToString()) :: movesListp1_k
          | 3 -> movesListp1_k <- (orgPosP1_k,"d"+(fst(((fst((_p1List_k).Head)).[i]))).ToString()) :: movesListp1_k
          | 4 -> movesListp1_k <- (orgPosP1_k,"e"+(fst(((fst((_p1List_k).Head)).[i]))).ToString()) :: movesListp1_k
          | 5 -> movesListp1_k <- (orgPosP1_k,"f"+(fst(((fst((_p1List_k).Head)).[i]))).ToString()) :: movesListp1_k
          | 6 -> movesListp1_k <- (orgPosP1_k,"g"+(fst(((fst((_p1List_k).Head)).[i]))).ToString()) :: movesListp1_k
          | 7 -> movesListp1_k <- (orgPosP1_k,"h"+(fst(((fst((_p1List_k).Head)).[i]))).ToString()) :: movesListp1_k
          | _ -> ()                       
 
    if not((_p1List_r).IsEmpty) then
      for i = 0 to (fst((_p1List_r).Head)).Length-1 do
        match (snd(((fst((_p1List_r).Head)).[i]))) with
          0 -> movesListp1_r <- (orgPosP1_r, "a"+(fst(((fst((_p1List_r).Head)).[i]))).ToString()) :: movesListp1_r
          | 1 -> movesListp1_r <- (orgPosP1_r,"b"+(fst(((fst((_p1List_r).Head)).[i])).ToString())) :: movesListp1_r
          | 2 -> movesListp1_r <- (orgPosP1_r,"c"+(fst(((fst((_p1List_r).Head)).[i]))).ToString()) :: movesListp1_r
          | 3 -> movesListp1_r <- (orgPosP1_r,"d"+(fst(((fst((_p1List_r).Head)).[i]))).ToString()) :: movesListp1_r
          | 4 -> movesListp1_r <- (orgPosP1_r,"e"+(fst(((fst((_p1List_r).Head)).[i]))).ToString()) :: movesListp1_r
          | 5 -> movesListp1_r <- (orgPosP1_r,"f"+(fst(((fst((_p1List_r).Head)).[i]))).ToString()) :: movesListp1_r
          | 6 -> movesListp1_r <- (orgPosP1_r,"g"+(fst(((fst((_p1List_r).Head)).[i]))).ToString()) :: movesListp1_r
          | 7 -> movesListp1_r <- (orgPosP1_r,"h"+(fst(((fst((_p1List_r).Head)).[i]))).ToString()) :: movesListp1_r
          | _ -> () 

    if not((_p2List_K).IsEmpty) then
      for i = 0 to (fst((_p2List_K).Head)).Length-1 do
        match (snd(((fst((_p2List_K).Head)).[i]))) with
          0 -> movesListp2_K <- (orgPosP2_K, "a"+(fst(((fst((_p2List_K).Head)).[i]))).ToString()) :: movesListp2_K
          | 1 -> movesListp2_K <- (orgPosP2_K, "b"+(fst(((fst((_p2List_K).Head)).[i])).ToString())) :: movesListp2_K
          | 2 -> movesListp2_K <- (orgPosP2_K, "c"+(fst(((fst((_p2List_K).Head)).[i]))).ToString()) :: movesListp2_K
          | 3 -> movesListp2_K <- (orgPosP2_K, "d"+(fst(((fst((_p2List_K).Head)).[i]))).ToString()) :: movesListp2_K
          | 4 -> movesListp2_K <- (orgPosP2_K, "e"+(fst(((fst((_p2List_K).Head)).[i]))).ToString()) :: movesListp2_K
          | 5 -> movesListp2_K <- (orgPosP2_K, "f"+(fst(((fst((_p2List_K).Head)).[i]))).ToString()) :: movesListp2_K
          | 6 -> movesListp2_K <- (orgPosP2_K, "g"+(fst(((fst((_p2List_K).Head)).[i]))).ToString()) :: movesListp2_K
          | 7 -> movesListp2_K <- (orgPosP2_K, "h"+(fst(((fst((_p2List_K).Head)).[i]))).ToString()) :: movesListp2_K
          | _ -> () 

    if not((_p2List_R).IsEmpty) then
      for i = 0 to (fst((_p2List_R).Head)).Length-1 do
        match (snd(((fst((_p2List_R).Head)).[i]))) with
          0 -> movesListp2_R <- (orgPosP2_R, "a"+(fst(((fst((_p2List_R).Head)).[i]))).ToString()) :: movesListp2_R
          | 1 -> movesListp2_R <- (orgPosP2_R, "b"+(fst(((fst((_p2List_R).Head)).[i])).ToString())) :: movesListp2_R
          | 2 -> movesListp2_R <- (orgPosP2_R, "c"+(fst(((fst((_p2List_R).Head)).[i]))).ToString()) :: movesListp2_R
          | 3 -> movesListp2_R <- (orgPosP2_R, "d"+(fst(((fst((_p2List_R).Head)).[i]))).ToString()) :: movesListp2_R
          | 4 -> movesListp2_R <- (orgPosP2_R, "e"+(fst(((fst((_p2List_R).Head)).[i]))).ToString()) :: movesListp2_R
          | 5 -> movesListp2_R <- (orgPosP2_R, "f"+(fst(((fst((_p2List_R).Head)).[i]))).ToString()) :: movesListp2_R
          | 6 -> movesListp2_R <- (orgPosP2_R, "g"+(fst(((fst((_p2List_R).Head)).[i]))).ToString()) :: movesListp2_R
          | 7 -> movesListp2_R <- (orgPosP2_R, "h"+(fst(((fst((_p2List_R).Head)).[i]))).ToString()) :: movesListp2_R
          | _ -> ()                 

    // printfn "%A" movesListp1_k
    
    match this.player with
      P1 ->
        Console.WriteLine("P1 king available moves:" + movesListp1_k.ToString())
        Console.WriteLine("P1 rook available moves:" + movesListp1_r.ToString())
        let regws = RegularExpressions.Regex "\s+" 
        let regalpha = RegularExpressions.Regex "[a-h]"
        let regnum = RegularExpressions.Regex "[0-7]"
        let chosenPos = Console.ReadLine()

        if chosenPos="quit" then
          "quit"
        else
          if not(regws.IsMatch (chosenPos)) || not(regalpha.IsMatch (chosenPos)) || not(regnum.IsMatch (chosenPos)) then
            "wrong pos"
          else
            chosenPos

      | P2 ->
        Console.WriteLine("P2 king available moves:" + movesListp2_K.ToString())
        Console.WriteLine("P2 rook available moves:" + movesListp2_R.ToString()) //Jeg sidder næsten der vi sad i går
        let regws = RegularExpressions.Regex "\s+" 
        let regalpha = RegularExpressions.Regex "[a-h]"
        let regnum = RegularExpressions.Regex "[0-7]"
        let chosenPos = Console.ReadLine()

        if chosenPos = "quit" then
          "quit"
        else
          if not(regws.IsMatch (chosenPos)) || not(regalpha.IsMatch (chosenPos)) || not(regnum.IsMatch (chosenPos)) then
            "wrong pos"
          else
            chosenPos


type Game()=

  let func(b:Board, chosenPos: string)=

    let regws = RegularExpressions.Regex "\s+" 

    let rr = regws.Split (chosenPos)

    let mutable orgX=0
    let mutable orgY=0
    let mutable finlX=0
    let mutable finlY=0

    match rr.[0].[0] with
          'a' ->  orgY <- 0
          | 'b' -> orgY <- 1
          | 'c' -> orgY <- 2
          | 'd' -> orgY <- 3
          | 'e' -> orgY <- 4
          | 'f' -> orgY <- 5
          | 'g' -> orgY <- 6
          | 'h' -> orgY <- 7
          | _ -> () 

    match rr.[0].[1] with
          '0' ->  orgX <- 0
          | '1' -> orgX <- 1
          | '2' -> orgX <- 2
          | '3' -> orgX <- 3
          | '4' -> orgX <- 4
          | '5' -> orgX <- 5
          | '6' -> orgX <- 6
          | '7' -> orgX <- 7 
          | _ -> () 

    match rr.[1].[1] with
          '0' ->  finlX <- 0
          | '1' -> finlX <- 1
          | '2' -> finlX <- 2
          | '3' -> finlX <- 3
          | '4' -> finlX <- 4
          | '5' -> finlX <- 5
          | '6' -> finlX <- 6
          | '7' -> finlX <- 7
          | _ -> () 

    match rr.[1].[0] with
          'a' ->  finlY <- 0
          | 'b' -> finlY <- 1
          | 'c' -> finlY <- 2
          | 'd' -> finlY <- 3
          | 'e' -> finlY <- 4
          | 'f' -> finlY <- 5
          | 'g' -> finlY <- 6
          | 'h' -> finlY <- 7
          | _ -> () 
        
       
    b.move (orgX,orgY) (finlX,finlY)

    printfn "%A" b

  member this.run(b : Board)=

    let player1 = new Human(P1)
    let player2 = new Human(P2)
  

    let mutable chosenPos = player1.nextMove(b)
    let mutable num=0

    while chosenPos="wrong pos" do
      printfn "%A" "wrong position, please try again"
      chosenPos <- player1.nextMove(b)
   

    while not(chosenPos="quit") do

      func(b, chosenPos)
      if (num=0) then
        chosenPos <- player2.nextMove(b)
        if chosenPos="wrong pos" then
          while chosenPos="wrong pos" do
            printfn "%A" "wrong position, please try again"
            chosenPos <- player2.nextMove(b)
        num <- 1
      elif (num=1) then
        chosenPos <- player1.nextMove(b)
        if chosenPos="wrong pos" then
          while chosenPos="wrong pos" do
            printfn "%A" "wrong position, please try again"
            chosenPos <- player1.nextMove(b)
        num <- 0