//$ fsharpc -a integrate.fsiintegrate.fs
//$ fsharpc -r integrate.dllapplication.fsx

type pit = int
type board = int []
type player = Player1 | Player2


let player1 = Player1
let player2 = Player2
 
// let b : board = ([|3;3;3;3;3;3;0;3;3;3;3;3;3;0|])

/// 7g0 - isGameOver

let printBoard (b : board) : unit =
   
    printfn "\n"
    printfn "\t %-5d %-5d %-5d %-5d %-5d %-5d" (b).[12] (b).[11] (b).[10] (b).[9] (b).[8] (b).[7] 
    printfn "%A \t \t \t \t \t \t%A" (b).[13] (b).[6]  
    printfn "\t %-5d %-5d %-5d %-5d %-5d %-5d" (b).[0] (b).[1] (b).[2] (b).[3] (b).[4] (b).[5]
    printfn "\n"

let distribute (b : board) (p : player) (i : pit) : board * player * pit =
  match p with 
    Player1 ->
        let mutable lastPit = 0
        if not ((b).[i] = 0) then   
            for j in 1 .. ((b).[i]) do
              if not ((b).[i] = 0) then (*WB: 1*) 
                (b).[i] <- (b).[i] - 1
                (b).[i+j] <- (b).[i+j] + 1
                lastPit <- (i+j)
                if ((i+j) = 13) && ((b).[i] > 0) then (*WB: 1*)
                  let mutable startIndex = 0
                  for k in 0 .. ((b).[i] - 1) do
                    (b).[i] <- (b).[i] - 1
                    (b).[startIndex+k] <- (b).[startIndex + k] + 1
                    lastPit <- startIndex + k
        if (b.[lastPit] = 1) && (not(lastPit = 6)) && (not(lastPit = 13)) && (not(b.[12 - lastPit] = 0)) then (*WB: 1*)
            b.[lastPit] <- b.[lastPit] - 1
            b.[6] <- b.[6] + b.[12 - lastPit] + 1
            b.[12-lastPit] <- 0
          
        (b, Player1, (lastPit))

    | Player2 ->
        let mutable lastPit = 0
        if not ((b).[i] = 0) then
            for j in 1 .. ((b).[i]) do
              if not ((b).[i] = 0) then (*WB: 2*)
                (b).[i] <- (b).[i] - 1
                (b).[i+j] <- (b).[i+j] + 1
                lastPit <- (i+j)
                if ((i+j) = 13) && ((b).[i] > 0) then (*WB: 2*)
                  let mutable startIndex = 0
                  for k in 0 .. ((b).[i] - 1) do
                    (b).[i] <- (b).[i] - 1
                    (b).[startIndex+k] <- (b).[startIndex + k] + 1
                    lastPit <- startIndex + k
       
        if (b.[lastPit] = 1) && (not (lastPit = 6)) && (not (lastPit = 13)) && (not(b.[(12 - lastPit)] = 0)) then (*WB: 2*) 
            b.[lastPit] <- b.[lastPit] - 1 
            b.[13] <- b.[13] + b.[12-lastPit] + 1 
            b.[12-lastPit] <- 0 
        (b, Player2, (lastPit))

let rec getMove (b : board) (p : player) (q : string) : pit =
  System.Console.WriteLine(q)            
  let index = (int (System.Console.ReadLine()))                 
  match p with
    Player1 ->
      if index > -1 && index < 6 && (not(b.[index]=0)) then 
        index  
      else  
        match b.[index] with
          0 -> printfn "%A" "pit empty choose again"
          | _-> printfn "%A" "wrong value"
        getMove b p q 
        
    | Player2 ->
        if index > - 1 && index < 6 && (not (b.[index+7] = 0)) then 
          (index+7)
        else 
          match b.[index + 7] with
            0 -> printfn "%A" "pit empty choose again" 
            | _-> printfn "%A" "wrong value"
          getMove b p q // recursive function
          

let isHome (b : board) (p : player) (i : pit) : bool =
  match p with 
    Player1 ->
      match i with 
        6 -> true (*WB: 1*)
        | _ -> false (*WB: 1*)
    | Player2 ->
      match i with 
        13 -> true(*WB: 2*)
        | _ -> false(*WB: 2*)

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
      if n = 0 then 
        sprintf "%A's move?" p
      else 
        "Again?"
    let i = getMove b p str
    let (newB, finalPitsPlayer, finalPit) = distribute b p i
    if not (isHome b finalPitsPlayer finalPit) 
       || (isGameOver b) then
      newB 
    else 
      repeat newB p (n + 1)
  repeat b p 0 

let rec play (b : board) (p : player) : board =
  if isGameOver b then 
    match p with 
      Player1 -> printfn "%s" ("player 2 won"); printBoard(b) (*WB: 1*)
      | Player2 -> printfn "%s" ("player 1 won"); printBoard(b) (*WB: 2*)
    b
  else 
    let newB = turn b p
    let nextP =
      if p = Player1 then 
        Player2
      else 
        Player1
    play newB nextP

printfn "   Unit: turn" 

printfn "\n White-box testing of 7g"
printfn "   Unit: play" 
printfn "       Branch: 1 - %b \n" ((play [|3;3;3;3;3;3;6;0;0;0;0;0;0;8|] player1) = (play [|3;3;3;3;3;3;6;0;0;0;0;0;0;8|] player1))
printfn "       Branch: 2 - %b \n" ((play [|0;0;0;0;0;0;8;3;3;3;3;3;3;6|] player2) = (play [|0;0;0;0;0;0;8;3;3;3;3;3;3;6|] player2))

printfn "\n Unit: isGameOver" 
printfn "       Branch: 1 - %b \n" ((isGameOver [|3;3;3;3;3;3;6;0;0;0;0;0;0;8|]) = true)
printfn "       Branch: 2 - %b \n" ((isGameOver [|1;2;3;4;5;2;8;3;3;3;3;3;3;6|]) = false)

printfn "\n Unit: isHome" 
printfn "       Branch: 1a - %b \n" ((isHome [|3;3;3;3;3;3;6;0;0;0;0;0;0;8|] player1 6) = true)
printfn "       Branch: 1b - %b \n" ((isHome [|1;2;3;4;5;2;8;3;3;3;3;3;3;6|] player1 2) = false)
printfn "       Branch: 2a - %b \n" ((isHome [|3;3;3;3;3;3;6;0;0;0;0;0;0;8|] player2 13) = true)
printfn "       Branch: 2b - %b \n" ((isHome [|1;2;3;4;5;2;8;3;3;3;3;3;3;6|] player2 8) = false)

printfn "\n Unit: distribute"
printfn "       Branch: 1a - %b \n" ((distribute [|3;3;3;3;3;3;6;0;0;0;0;0;0;8|] player1 2) = ([|3;3;0;4;4;4;6;0;0;0;0;0;0;8|], player1, 5))
printfn "       Branch: 1b - %b \n" ((distribute [|3;3;3;3;3;9;6;0;0;0;0;0;0;8|] player1 5) = ([|4;3;3;3;3;0;7;1;1;1;1;1;1;9|], player1, 0))
printfn "       Branch: 1c - %b \n" ((distribute [|1;0;3;3;3;3;0;3;3;3;3;3;3;0|] player1 0) = ([|0;0;3;3;3;3;4;3;3;3;3;0;3;0|], player1, 1))
printfn "       Branch: 2a - %b \n" ((distribute [|0;0;0;0;0;0;0;8;3;3;3;3;3;3|] player2 9) = ([|0;0;0;0;0;0;0;8;3;0;4;4;4;3|], player2, 12))
printfn "       Branch: 2b - %b \n" ((distribute [|0;0;0;0;0;0;6;8;3;3;3;3;9;0|] player2 12) = ([|1;1;1;1;1;1;7;9;3;3;3;3;0;1|], player2, 7))
printfn "       Branch: 2c - %b \n" ((distribute [|3;3;3;3;3;3;3;1;0;3;3;3;3;0|] player2 7) = ([|3;3;3;3;0;3;3;0;0;3;3;3;3;4|], player2, 8))
