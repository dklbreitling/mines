package minesweeperlikegamepleasedontsuememichaelsoft.logic

import minesweeperlikegamepleasedontsuememichaelsoft.logic.GameState._

case class GameState(
                    gridDims: Dimensions,
                    board: Seq[Seq[Tile]],
                    numMines: Int,
                    gameOver: Boolean = false,
                    initialTile: Boolean = true
                    ) {}

object GameState {}
