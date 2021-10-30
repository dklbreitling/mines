package minesweeperlikegamepleasedontsuememichaelsoft.logic

import engine.random.{RandomGenerator, ScalaRandomGen}
import minesweeperlikegamepleasedontsuememichaelsoft.logic.MinesweeperLogic._

class MinesweeperLogic(val randomGen: RandomGenerator,
                       val gridDims: Dimensions,
                       val initialBoard: Seq[Seq[Tile]]) {

	def this(random: RandomGenerator, gridDims: Dimensions) =
		this(random, gridDims, makeEmptyBoard(gridDims))

	def this() =
		this(new ScalaRandomGen(), DefaultDims, makeEmptyBoard(DefaultDims))

	var currentGameState: GameState = GameState(gridDims, initialBoard)

	def generateBomb(): Tile =
		Tile(Bomb)

	def uncoverTile(point: Point) : Unit =
	{
		currentGameState = currentGameState.copy(board = addTileToBoardAtPoint(point, Tile(Visible)))
	}

	def addTileToBoardAtPoint(point: Point, tile: Tile): Seq[Seq[Tile]] =
		currentGameState.board.updated(point.y, currentGameState.board(point.y).updated(point.x, tile))

	def isGameOver: Boolean = currentGameState.gameOver

	def getTile(p: Point): Tile =
	{
		currentGameState.board(p.y)(p.x)
	}
}

object MinesweeperLogic {

	val TetrominoStartingRowIndex = 1
	val NrTetrominoes = 7

	def makeEmptyBoard(gridDims: Dimensions): Seq[Seq[Tile]] =
	{
		val emptyLine = Seq.fill(gridDims.width)(Tile(Hidden))
		Seq.fill(gridDims.height)(emptyLine)
	}

	val FramesPerSecond: Int = 3
	val DrawSizeFactor = 2.0
	val DefaultWidth: Int = 20
	val DefaultHeight: Int = 20
	val DefaultDims: Dimensions = Dimensions(width = DefaultWidth, height = DefaultHeight)

	def apply() = new MinesweeperLogic(new ScalaRandomGen(),
	                                   DefaultDims,
	                                   makeEmptyBoard(DefaultDims))
}