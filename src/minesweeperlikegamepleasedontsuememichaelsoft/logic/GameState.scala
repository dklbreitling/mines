package minesweeperlikegamepleasedontsuememichaelsoft.logic

import minesweeperlikegamepleasedontsuememichaelsoft.logic.GameState._

case class GameState(
                    gridDims: Dimensions,
                    board: Seq[Seq[Tile]],
                    gameOver: Boolean = false
                    ) {}

object GameState {}
