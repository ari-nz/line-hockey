dir.Ai <- "./AI"
dir.Board <- "./Board" 
dir.Game <- "./Game"


source(file.path(dir.Game, "MakeMove.R"))
source(file.path(dir.Game, "GetAvailableMoves.R"))
source(file.path(dir.Game, "ContinueTurn.R"))
source(file.path(dir.Board, "CreateGameBoard.R"))
source(file.path(dir.Board, "PlotMove.R"))

GameState = CreateBoard(5)


if(!interactive()){
  print(GameState$MoveMatrix)
  MakeMove(7, 8, GameState)
  PlotMove(7, 8, GameState$GamePoints)

  GetAvailableMoves(7, GameState)
}




