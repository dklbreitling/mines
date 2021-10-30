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

	for (_ <- 0 until DefaultNumMines) {
		val p = currentlyFreeTiles(randomGen.randomInt(currentlyFreeTiles.size))
		currentGameState = currentGameState.copy(board = addTileToBoardAtPoint(p, newBomb))
		incrementCountersAroundPoint(p)
	}

	def incrementCountersAroundPoint(point: Point): Unit =
	{
		val neighbors: Seq[Point] = NeighborOffsets.map(_ + point)
		for (p <- neighbors if gridDims.allPointsInside.contains(p)) {
			incrementCounter(p)
		}
	}

	def incrementCounter(point: Point): Unit =
	{
		currentGameState = currentGameState.copy(board = addTileToBoardAtPoint(point, getTile(point).copy(count = getTile(point).count + 1)))
	}

	def currentlyFreeTiles: Seq[Point] =
	{
		for {
			p <- gridDims.allPointsInside
			if getTile(p).cellType == Hidden && !getTile(p).hasBomb
		} yield p
	}

	def newBomb: Tile = Tile(Hidden, hasBomb = true)

	def uncoverTile(point: Point): Unit =
	{
		currentGameState = currentGameState.copy(board = addTileToBoardAtPoint(point, getTile(point).copy(cellType = Visible)))
		if (getTile(point).hasBomb)
			currentGameState = currentGameState.copy(gameOver = true)
	}

	def addFlag(point: Point): Unit =
	{
		currentGameState = currentGameState.copy(board = addTileToBoardAtPoint(point, getTile(point).copy(cellType = Flag)))
	}

	def addTileToBoardAtPoint(point: Point, tile: Tile): Seq[Seq[Tile]] =
		currentGameState.board.updated(point.y, currentGameState.board(point.y).updated(point.x, tile))

	def isGameOver: Boolean = currentGameState.gameOver

	def uncoverAllBombs(): Unit =
	{
		for (p <- gridDims.allPointsInside if getTile(p).hasBomb) {
			uncoverTile(p)
		}
	}

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
	val DrawSizeFactor = 3.0
	val DefaultWidth: Int = 16
	val DefaultHeight: Int = 16
	val DefaultNumMines: Int = 40
	val DefaultDims: Dimensions = Dimensions(width = DefaultWidth, height = DefaultHeight)

	def apply() = new MinesweeperLogic(new ScalaRandomGen(),
	                                   DefaultDims,
	                                   makeEmptyBoard(DefaultDims))

	val NeighborOffsets: Seq[Point] = Seq(Point(-1, -1), Point(-1, 0), Point(-1, 1), Point(1, 0), Point(0, 1), Point(0, -1), Point(1, 1), Point(1, -1))
}