open AwariGame

//$ fsharpc -a integrate.fsiintegrate.fs
//$ fsharpc -r integrate.dllapplication.fsx

type board = int []

let player1 = Player1
let player2 = Player2

// let b : board = ([|3;3;3;3;3;3;0;3;3;3;3;3;3;0|])
// (play b player1)  

let b : board = ([|0;0;0;0;0;1;10;0;0;0;0;0;1;11|])
(play b player1)  

