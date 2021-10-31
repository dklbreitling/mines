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

	var currentGameState: GameState = GameState(gridDims, initialBoard, DefaultNumMines)

	for (_ <- 0 until currentGameState.numMines) {
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
		if (currentGameState.initialTile) {
			currentGameState = currentGameState.copy(initialTile = false)
			val numTilesToUncover = randomGen.randomInt((gridDims.width * gridDims.height * 0.2).toInt)
			var neighbors = NeighborOffsets.map(_ + point).filter(p => gridDims.allPointsInside.contains(p))
			uncoverTilesAroundPoint(point)
		} else {
			openTile(point)
			if (getTile(point).hasBomb)
				currentGameState = currentGameState.copy(gameOver = true)
		}
	}

	def uncoverTilesAroundPoint(point: Point) : Unit =
	{
		openTile(point)
		val neighborsWithBombs: Seq[Point] = uncoverAllNeighborsAndReturnNeighborsWithBombs(point)
		for (p <- neighborsWithBombs)
			uncoverAllNeighborsAndReturnNeighborsWithBombs(p)
	}

	def uncoverAllNeighborsAndReturnNeighborsWithBombs(point: Point): Seq[Point] =
	{
		var neighborsWithBombs : Seq[Point] = Seq()
		for (p <- NeighborOffsets.map(_ + point).filter(p => gridDims.allPointsInside.contains(p))) {
			if (!getTile(p).hasBomb)
				openTile(p)
			else
				neighborsWithBombs = neighborsWithBombs.appended(p)
		}
		neighborsWithBombs
	}

	def openTile(point: Point): Unit =
		currentGameState = currentGameState.copy(board = addTileToBoardAtPoint(point, getTile(point).copy(cellType = Visible)))

	def flag(point: Point): Unit =
	{
		if (getTile(point).cellType == Flag)
			currentGameState = currentGameState.copy(board = addTileToBoardAtPoint(point, getTile(point).copy(cellType = Hidden)))
		else
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

	val EasyWidth: Int = 8
	val EasyHeight: Int = 8
	val EasyNumMines: Int = 10
	val EasyDims: Dimensions = Dimensions(width = EasyWidth, height = EasyHeight)

	val HardWidth: Int = 30
	val HardHeight: Int = 16
	val HardNumMines: Int = 99
	val HardDims: Dimensions = Dimensions(width = HardWidth, height = HardHeight)

	def apply() = new MinesweeperLogic(new ScalaRandomGen(),
	                                   DefaultDims,
	                                   makeEmptyBoard(DefaultDims))

	val NeighborOffsets: Seq[Point] = Seq(Point(-1, -1), Point(-1, 0), Point(-1, 1), Point(1, 0), Point(0, 1), Point(0, -1), Point(1, 1), Point(1, -1))
}