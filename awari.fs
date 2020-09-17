module AwariGame
type pit = int
type board = int[]
type player = Player1 | Player2

// The documentary standard can be found in "awari.fsi" 

let printBoard (b : board) : unit =
   
    printfn "\n"
    printfn "\t %-5d %-5d %-5d %-5d %-5d %-5d" (b).[12] (b).[11] (b).[10] (b).[9] (b).[8] (b).[7] 
    printfn "%A \t \t \t \t \t \t%A" (b).[13] (b).[6]  
    printfn "\t %-5d %-5d %-5d %-5d %-5d %-5d" (b).[0] (b).[1] (b).[2] (b).[3] (b).[4] (b).[5]
    printfn "\n"

let distribute (b : board) (p : player) (i : pit) : board * player * pit =
  match p with (*WB: 0*)
    Player1 ->
        let mutable lastPit = 0
        if not ((b).[i] = 0) then   (*WB: 1*)
            for j in 1 .. ((b).[i]) do
              if not ((b).[i] = 0) then (*WB: 2*) // if sætning er nødventig til at stoppe ydre løkken ellers kommer der indexoutofrange exception 
                (b).[i] <- (b).[i] - 1
                (b).[i+j] <- (b).[i+j] + 1
                lastPit <- (i+j)
                if ((i+j) = 13) && ((b).[i] > 0) then (*WB: 3*)
                  for k in 0 .. ((b).[i] - 1) do
                    (b).[i] <- (b).[i] - 1
                    (b).[k] <- (b).[k] + 1
                    lastPit <- k
        if (b.[lastPit] = 1) && (not(lastPit = 6)) && (not(lastPit = 13)) then (*WB: 4*) //capturing. Tester hvis sidste bøn lander i et tom felt som ikke er et hjemmefelt.
           //tester hvis feltet overfor ikke er tom hvis bønen lander i 0 .. 5
          if (not (b.[12 - lastPit] = 0)) then  (*WB: 5*)
            b.[lastPit] <- b.[lastPit] - 1 // sidste bøn flyttes
            b.[6] <- b.[6] + b.[12 - lastPit] + 1 // feltet overfor flyttes til hjemmefeltet + sidste bøn 
            b.[12-lastPit] <- 0 // feltet overfor fanges og stilles til 0
          
        (b, Player1, (lastPit))

    | Player2 ->
        let mutable lastPit = 0
        if not ((b).[i] = 0) then (*WB: 1*)
            for j in 1 .. ((b).[i]) do
              if not ((b).[i] = 0) then (*WB: 2*)
                (b).[i] <- (b).[i] - 1
                (b).[i+j] <- (b).[i+j] + 1
                lastPit <- (i+j)
                if ((i+j) = 13) && ((b).[i] > 0) then (*WB: 3*)
                  for k in 0 .. ((b).[i] - 1) do
                    (b).[i] <- (b).[i] - 1
                    (b).[k] <- (b).[k] + 1
                    lastPit <- k
        if (b.[lastPit] = 1) && (not (lastPit = 6)) && (not (lastPit = 13)) then (*WB: 4*) //tester hvis b'nen lander i et hjemmefelt (lastpit=1 fordi en bøn er lagt til i løkken overfor)
          if (not(b.[(12 - lastPit)] = 0)) then (*WB: 5*) //tester hvis feltet overfor ikke er tom hvis bønen lander i 0 .. 5
            b.[lastPit] <- b.[lastPit] - 1 // sidste bøn flyttes
            b.[13] <- b.[13] + b.[12-lastPit] + 1 // feltet overfor flyttes til hjemmefeltet + sidste bøn 
            b.[12-lastPit] <- 0 // feltet overfor fanges og stilles til 0
        (b, Player2, (lastPit))

let rec getMove (b : board) (p : player) (q : string) : pit =
  System.Console.WriteLine(q)
  try
    let index = (int (System.Console.ReadLine()))    
          
    match p with (*WB: 0*)
    Player1 ->
      if index > -1 && index < 6 && (not(b.[index]=0)) then (*WB: 1*)
        index
      else (*WB: 2*) 
        match b.[index] with
          0 -> printfn "%s" "Pit empty choose again"
          | _-> printfn "%s" "Wrong value"

        getMove b p q // recursive function getMove (b player1 q)
        
    | Player2 ->
        if index > - 1 && index < 6 && (not (b.[index+7] = 0)) then (*WB: 1*)
          (index+7)
        else (*WB: 2*)
          try
            match b.[index + 7] with
              0 -> printfn "%s" "Pit empty choose again" 
              | _-> printfn "%s" "Wrong value"
            getMove b p q // recursive function
          with
            :? System.IndexOutOfRangeException as ex ->
              getMove b p "Wrong value"
  with
    | :? System.FormatException as ex ->
      getMove b p "Invalid input. Please enter a int" 

          

let isHome (b : board) (p : player) (i : pit) : bool =
  match p with (*WB: 0*)
    Player1 ->
      match i with (*WB: 1*)
        6 -> true
        | _ -> false
    | Player2 ->
      match i with (*WB: 2*)
        13 -> true
        | _ -> false

let isGameOver (b : board) : bool =
  let mutable array1 = [||]
  for i in 0 .. 5 do
    array1 <- Array.append [|b.[i];|] array1

  let mutable array2 = [||]
  for i in 7 .. 12 do
    array2 <- Array.append [|b.[i];|] array2

  if (Array.forall (fun x -> x = 0) array1) || (Array.forall (fun x -> x = 0) array2) then (*WB: 1*)
    true
  else (*WB: 2*)
    false

let turn (b : board) (p : player) : board =
  let rec repeat (b: board) (p: player) (n: int) : board =
    printBoard b  
    let str =
      if n = 0 then (*WB: 1*)
        sprintf "%A's move?" p
      else (*WB: 2*)
        "Again?"
    let i = getMove b p str
    let (newB, finalPitsPlayer, finalPit) = distribute b p i
    if not (isHome b finalPitsPlayer finalPit) (*WB: 3*)
       || (isGameOver b) then
      newB
    else (*WB: 4*)
      repeat newB p (n + 1)
  repeat b p 0 

let rec play (b : board) (p : player) : board =
  if isGameOver b then 
    if b.[6]>b.[13] then
      printfn "%s" ("Player 1 won!")
    elif (b.[13]>b.[6]) then
      printfn "%s" ("Player 2 won!")
    elif (b.[13]=b.[6]) then
      printfn "%s" "It's a draw!"
   
    b
  else (*WB: 2*)
    let newB = turn b p
    let nextP =
      if p = Player1 then (*WB: 3*)
        Player2
      else (*WB: 4*)
        Player1
    play newB nextP

// fsc -a awari.fsi awari.fs
// fsc -r awari.dll awariapp.fsx