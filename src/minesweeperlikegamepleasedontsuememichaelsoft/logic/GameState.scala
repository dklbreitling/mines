package minesweeperlikegamepleasedontsuememichaelsoft.logic

import minesweeperlikegamepleasedontsuememichaelsoft.logic.GameState._

case class GameState(
                    gridDims: Dimensions,
                    board: Seq[Seq[Tile]],
                    numMines: Int,
                    gameOver: Boolean = false,
                    initialTileSet: Boolean = true,
                    initialTile: Point = Point(-1, -1)
                    ) {}

object GameState {}
