package minesweeperlikegamepleasedontsuememichaelsoft.logic

import engine.random.{RandomGenerator, ScalaRandomGen}
import minesweeperlikegamepleasedontsuememichaelsoft.logic.MinesweeperLogic._

class MinesweeperLogic(val randomGen: RandomGenerator,
                       val gridDims: Dimensions,
                       val initialBoard: Seq[Seq[Tile]],
                       val inMenu: Boolean) {

	def this() =
		this(new ScalaRandomGen(), DefaultDims, makeEmptyBoard(DefaultDims), true)

	var currentGameState: GameState = GameState(gridDims, initialBoard, if (gridDims == EasyDims) EasyNumMines else if (gridDims == HardDims) HardNumMines else DefaultNumMines, isInMenu = inMenu)

	def isGameRunning: Boolean = !(currentGameState.gameOver || (hasWon && currentGameState.initialTileSet)) && !currentGameState.isInMenu

	def isInMenu: Boolean = currentGameState.isInMenu

	def generateMines(): Unit =
	{
		for (_ <- 0 until currentGameState.numMines) {
			val freeTiles = currentlyFreeTiles.filter(tile => !neighborsOfPoint(currentGameState.initialTile).contains(tile))
			val p = freeTiles(randomGen.randomInt(freeTiles.size))
			currentGameState = currentGameState.copy(board = addTileToBoardAtPoint(p, newBomb))
			incrementCountersAroundPoint(p)
		}
	}

	def incrementCountersAroundPoint(point: Point): Unit =
	{
		for (p <- neighborsOfPoint(point))
			incrementCounter(p)
	}

	def incrementCounter(point: Point): Unit =
		currentGameState = currentGameState.copy(board = addTileToBoardAtPoint(point, getTile(point).copy(count = getTile(point).count + 1)))

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
		if (currentGameState.initialTileSet) {
			currentGameState = currentGameState.copy(initialTileSet = false, initialTile = point)
			openTile(point)
			generateMines()
		}
		if (getTile(point).hasBomb) {
			currentGameState = currentGameState.copy(gameOver = true)
			openTile(point)
		} else if (getTile(point).count == 0) {
			recursivelyUncoverNeighbors(point)
		} else {
			openTile(point)
		}
	}

	def recursivelyUncoverNeighbors(point: Point): Unit =
	{
		openTile(point)
		for (p <- neighborsOfPoint(point)) {
			if (!getTile(p).hasBomb && getTile(p).cellType != Visible && !getTile(p).hasFlag) {
				if (getTile(p).count == 0)
					recursivelyUncoverNeighbors(p)
				else
					openTile(p)
			}
		}
	}

	def hasWon: Boolean =
		allHiddenBombs.forall(p => getTile(p).hasFlag) && currentlyFreeTiles.isEmpty

	def neighborsOfPoint(point: Point): Seq[Point] =
		NeighborOffsets.map(_ + point).filter(p => gridDims.allPointsInside.contains(p))

	def uncoverAllNeighbors(point: Point): Seq[Point] =
	{
		var neighborsWithBombs: Seq[Point] = Seq()
		for (p <- neighborsOfPoint(point)) {
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
		currentGameState = currentGameState.copy(board = addTileToBoardAtPoint(point, getTile(point).copy(hasFlag = !getTile(point).hasFlag)))

	def addTileToBoardAtPoint(point: Point, tile: Tile): Seq[Seq[Tile]] =
		currentGameState.board.updated(point.y, currentGameState.board(point.y).updated(point.x, tile))

	def isGameOver: Boolean = currentGameState.gameOver

	def uncoverAllBombs(): Unit =
	{
		for (p <- gridDims.allPointsInside if getTile(p).hasBomb) {
			uncoverTile(p)
		}
	}

	def uncoverNextBomb(): Unit =
	{
		if (allHiddenBombs.nonEmpty)
			openTile(allHiddenBombs(randomGen.randomInt(allHiddenBombs.size)))
	}

	def allHiddenBombs: Seq[Point] =
	{
		for {
			p <- gridDims.allPointsInside
			if getTile(p).cellType == Hidden && getTile(p).hasBomb
		} yield p
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
	                                   makeEmptyBoard(DefaultDims),
	                                   true)

	val NeighborOffsets: Seq[Point] = Seq(Point(-1, -1), Point(-1, 0), Point(-1, 1), Point(1, 0), Point(0, 1), Point(0, -1), Point(1, 1), Point(1, -1))
}