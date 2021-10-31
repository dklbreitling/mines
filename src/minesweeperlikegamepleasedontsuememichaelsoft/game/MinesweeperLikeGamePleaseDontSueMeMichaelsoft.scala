package minesweeperlikegamepleasedontsuememichaelsoft.game

import engine.GameBase
import engine.graphics.{Color, Point, Rectangle}
import minesweeperlikegamepleasedontsuememichaelsoft.game.MinesweeperLikeGamePleaseDontSueMeMichaelsoft._
import minesweeperlikegamepleasedontsuememichaelsoft.logic.{Point => GridPoint, _}
import processing.core.{PApplet, PConstants}
import processing.event.KeyEvent

import java.awt.event

class MinesweeperLikeGamePleaseDontSueMeMichaelsoft extends GameBase {

	var gameLogic: MinesweeperLogic = MinesweeperLogic()
	val updateTimer = new UpdateTimer(MinesweeperLogic.FramesPerSecond.toFloat)
	val gridDims: Dimensions = gameLogic.gridDims
	val widthInPixels: Int = (WidthCellInPixels * gridDims.width).ceil.toInt
	val heightInPixels: Int = (HeightCellInPixels * gridDims.height).ceil.toInt
	val screenArea: Rectangle = Rectangle(Point(0, 0), widthInPixels.toFloat, heightInPixels.toFloat)
	var gameOver: Boolean = false

	override def draw(): Unit =
	{
		updateState()
		drawGrid()
		if (gameLogic.isGameOver) {
			gameLogic.uncoverNextBomb()
			drawGameOverScreen()
			gameOver = true
		}
	}

	def drawGameOverScreen(): Unit =
	{
		setFillColor(Color.Red)
		drawTextCentered("GAME OVER!", 20, screenArea.center)
	}

	def drawGrid(): Unit =
	{
		val widthPerCell = screenArea.width / gridDims.width
		val heightPerCell = screenArea.height / gridDims.height

		for (p <- gridDims.allPointsInside) {
			if (p == currentMousePositionAsPoint && !gameOver)
				drawCell(getCell(p), gameLogic.getTile(p), decreaseAlpha = true)
			else
				drawCell(getCell(p), gameLogic.getTile(p))
		}

		def getCell(p: GridPoint): Rectangle =
		{
			val leftUp = Point(screenArea.left + p.x * widthPerCell,
			                   screenArea.top + p.y * heightPerCell)
			Rectangle(leftUp, widthPerCell, heightPerCell)
		}

		def drawCell(area: Rectangle, tile: Tile, decreaseAlpha: Boolean = false): Unit =
		{
			val color = if (decreaseAlpha) Color.decreaseAlpha(cellTypeToColor(tile.cellType)) else cellTypeToColor(tile.cellType)
			setFillColor(color)
			drawRectangle(area)
			setFillColor(Color.Black)
			if (tile.hasFlag) {
				setFillColor(Color.Red)
				drawRectangle(area)
				setFillColor(Color.Black)
			}
			if (tile.cellType == Visible && tile.count > 0)
				drawTextCentered(tile.count.toString, size = 18, Point(area.center.x, area.center.y + 8))
			if (tile.hasBomb && tile.cellType != Hidden) {
				drawEllipse(area)
			}
		}
	}

	def currentMousePositionAsPoint: GridPoint =
		GridPoint(math.floor(mouseX / WidthCellInPixels).toInt, math.floor(mouseY / WidthCellInPixels).toInt)

	/** Method that calls handlers for different key press events.
	  * You may add extra functionality for other keys here.
	  * See [[event.KeyEvent]] for all defined keycodes.
	  *
	  * @param event The key press event to handle
	  */
	override def keyPressed(event: KeyEvent): Unit =
	{
		event.getKeyCode match {
			case _ => ()
		}
	}

	override def mouseClicked(): Unit =
	{
		if (!gameOver) {
			if (mouseButton == PConstants.LEFT)
				gameLogic.uncoverTile(currentMousePositionAsPoint)
			else if (mouseButton == PConstants.RIGHT)
				gameLogic.flag(currentMousePositionAsPoint)
		}
	}

	override def settings(): Unit =
	{
		pixelDensity(displayDensity())
		size(widthInPixels, heightInPixels, PConstants.P2D)
	}

	override def setup(): Unit =
	{
		// Fonts are loaded lazily, so when we call text()
		// for the first time, there is significant lag.
		// This prevents it from happening during gameplay.
		text("", 0, 0)

		// This should be called last, since the game
		// clock is officially ticking at this point
		updateTimer.init()
	}

	def updateState(): Unit =
	{
		if (updateTimer.timeForNextFrame()) {
			// gameLogic.moveDown()
			updateTimer.advanceFrame()
		}
	}

	def cellTypeToColor(color: CellType): Color =
		color match {
			case Hidden => Color.Gray
			case Visible => Color.White
		}
}

object MinesweeperLikeGamePleaseDontSueMeMichaelsoft {

	val WidthCellInPixels: Double = 15 * MinesweeperLogic.DrawSizeFactor
	val HeightCellInPixels: Double = WidthCellInPixels

	def main(args: Array[String]): Unit =
	{
		PApplet.main("minesweeperlikegamepleasedontsuememichaelsoft.game.MinesweeperLikeGamePleaseDontSueMeMichaelsoft")
	}
}
