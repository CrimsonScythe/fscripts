open Chess
open Pieces
/// Print various information about a piece
let printPiece (board : Board) (p : chessPiece) : unit =
  printfn "%A: %A %A" p p.position (p.availableMoves board)
  printfn "\n"

// Create a game
let board = Chess.Board () // Create a board
// Pieces are kept in an array for easy testing
let pieces = [|
  king (White) :> chessPiece;
  rook (White) :> chessPiece;
  king (Black) :> chessPiece |]
  // rook (Black) :> chessPiece |] 

// Place pieces on the board
// board.[0,3] <- Some pieces.[0]
// board.[7,4] <- Some pieces.[1]
// board.[0,7] <- Some pieces.[2]
// board.[7,0] <- Some pieces.[3]

board.[0,0] <- Some pieces.[0]
board.[1,1] <- Some pieces.[1]
board.[4,1] <- Some pieces.[2]

// board.[0,0] <- Some pieces.[0]
// board.[1,1] <- Some pieces.[3]
// board.[4,1] <- Some pieces.[2]
// board.[0,6] <- Some pieces.[1]
// printfn "%A" board 
// Array.iter (printPiece board) pieces

// Make moves
// board.move (0,6) (0,0) // Moves a piece from (1,1) to (3,1)


printfn "%A" board
Array.iter (printPiece board) pieces

let game = Chess.Game()
game.run (board)

/// fsc chess.fs pieces.fs chessApp.fsx && chessApp.exe