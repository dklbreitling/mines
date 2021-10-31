package minesweeperlikegamepleasedontsuememichaelsoft.game

import engine.GameBase
import engine.graphics.{Color, Point, Rectangle}
import engine.random.ScalaRandomGen
import minesweeperlikegamepleasedontsuememichaelsoft.game.MinesweeperLikeGamePleaseDontSueMeMichaelsoft._
import minesweeperlikegamepleasedontsuememichaelsoft.logic.{Point => GridPoint, _}
import processing.core.{PApplet, PConstants}
import processing.event.KeyEvent

import java.awt.event
import java.awt.event.KeyEvent._

class MinesweeperLikeGamePleaseDontSueMeMichaelsoft extends GameBase {

	var gameLogic: MinesweeperLogic = MinesweeperLogic()
	val updateTimer = new UpdateTimer(MinesweeperLogic.FramesPerSecond.toFloat)
	var gridDims: Dimensions = MinesweeperLogic.DefaultDims
	var widthInPixels: Int = (WidthCellInPixels * gridDims.width).ceil.toInt
	var heightInPixels: Int = (HeightCellInPixels * gridDims.height).ceil.toInt
	var screenArea: Rectangle = Rectangle(Point(0, 0), widthInPixels.toFloat, heightInPixels.toFloat)
	var gameOver: Boolean = false
	val topArea: Rectangle = Rectangle(Point(10, 10), widthInPixels.toFloat - 20, (heightInPixels.toFloat / 3) - 13.33f)
	val midArea: Rectangle = Rectangle(Point(10, (heightInPixels.toFloat / 3) + 6.67f), widthInPixels.toFloat - 20, (heightInPixels.toFloat / 3) - 13.33f)
	val bottomArea: Rectangle = Rectangle(Point(10, (2 * (heightInPixels.toFloat / 3)) + 3.33f), widthInPixels.toFloat - 20, (heightInPixels.toFloat / 3) - 13.33f)

	def reinitSize() : Unit =
	{
		widthInPixels = (WidthCellInPixels * gridDims.width).ceil.toInt
		heightInPixels = (HeightCellInPixels * gridDims.height).ceil.toInt
		screenArea = Rectangle(Point(0, 0), widthInPixels.toFloat, heightInPixels.toFloat)
		surface.setSize(widthInPixels, heightInPixels)
	}

	override def draw(): Unit =
	{
		if (gameLogic.isInMenu) {
			drawMenu()
		} else {
			updateState()
			drawGrid()
			if (gameLogic.isGameOver) {
				gameLogic.uncoverNextBomb()
				drawGameOverScreen()
				gameOver = true
			} else if (gameLogic.hasWon) {
				drawGameWonScreen()
			}
		}
	}

	def drawMenu(): Unit =
	{
		setFillColor(Color.Black)
		drawRectangle(screenArea)
		drawEasyBox()
		drawMediumBox()
		drawHardBox()
	}

	def drawEasyBox(): Unit =
	{
		setFillColor(Color.decreaseAlpha(Color.Gray))
		drawRectangle(topArea)
		setFillColor(Color.Red)
		drawTextCentered("EASY\n8x8, 10 mines", 25, Point(topArea.center.x, topArea.center.y + 8))
	}

	def drawMediumBox(): Unit =
	{
		setFillColor(Color.decreaseAlpha(Color.Gray))
		drawRectangle(midArea)
		setFillColor(Color.Red)
		drawTextCentered("MEDIUM\n16x16, 40 mines", 25, Point(midArea.center.x, midArea.center.y + 8))
	}

	def drawHardBox(): Unit =
	{
		setFillColor(Color.decreaseAlpha(Color.Gray))
		drawRectangle(bottomArea)
		setFillColor(Color.Red)
		drawTextCentered("HARD\n30x16, 99 mines", 25, Point(bottomArea.center.x, bottomArea.center.y + 8))
	}

	def drawGameOverScreen(): Unit =
	{
		setFillColor(Color.Red)
		drawTextCentered("GAME OVER!\nPress 'Q' to exit.", 20, screenArea.center)
	}

	def drawGameWonScreen(): Unit =
	{
		setFillColor(Color.Green)
		drawTextCentered("YOU WON!", 20, screenArea.center)
	}

	def drawGrid(): Unit =
	{
		val widthPerCell = screenArea.width / gridDims.width
		val heightPerCell = screenArea.height / gridDims.height

		if (!gameLogic.isInMenu) {
			for (p <- gameLogic.gridDims.allPointsInside) {
				if (p == currentMousePositionAsPoint && gameLogic.isGameRunning)
					drawCell(getCell(p), gameLogic.getTile(p), decreaseAlpha = true)
				else
					drawCell(getCell(p), gameLogic.getTile(p))
			}
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

	def isMouseInArea(area: Rectangle): Boolean =
	{
		mouseX >= area.leftUp.x && mouseY >= area.leftUp.y && mouseX <= area.leftUp.x + area.width && mouseY <= area.leftUp.y + area.height
	}

	/** Method that calls handlers for different key press events.
	  * You may add extra functionality for other keys here.
	  * See [[event.KeyEvent]] for all defined keycodes.
	  *
	  * @param event The key press event to handle
	  */
	override def keyPressed(event: KeyEvent): Unit =
	{
		event.getKeyCode match {
			case VK_Q =>
				if (!gameLogic.isGameRunning) {
					exit()
				}
			case _ => ()
		}
	}

	override def mouseClicked(): Unit =
	{
		if (gameLogic.isGameRunning) {
			if (mouseButton == PConstants.LEFT)
				gameLogic.uncoverTile(currentMousePositionAsPoint)
			else if (mouseButton == PConstants.RIGHT)
				gameLogic.flag(currentMousePositionAsPoint)
		} else if (gameLogic.isInMenu && mouseButton == PConstants.LEFT) {
			if (isMouseInArea(topArea)) {
				gameLogic = new MinesweeperLogic(new ScalaRandomGen(),
				                                 MinesweeperLogic.EasyDims,
				                                 MinesweeperLogic.makeEmptyBoard(MinesweeperLogic.EasyDims),
				                                 false)
				gridDims = MinesweeperLogic.EasyDims
				reinitSize()
			} else if (isMouseInArea(midArea)) {
				gameLogic = new MinesweeperLogic(new ScalaRandomGen(),
				                                 MinesweeperLogic.DefaultDims,
				                                 MinesweeperLogic.makeEmptyBoard(MinesweeperLogic.DefaultDims),
				                                 false)
				gridDims = MinesweeperLogic.DefaultDims
				reinitSize()
			} else if (isMouseInArea(bottomArea)) {
				gameLogic = new MinesweeperLogic(new ScalaRandomGen(),
				                                 MinesweeperLogic.HardDims,
				                                 MinesweeperLogic.makeEmptyBoard(MinesweeperLogic.HardDims),
				                                 false)
				gridDims = MinesweeperLogic.HardDims
				reinitSize()
			}
		}
	}

	override def settings(): Unit =
	{
		pixelDensity(displayDensity())
		size(widthInPixels, heightInPixels, PConstants.P2D)
	}

	override def setup(): Unit =
	{
		textMode(PConstants.SHAPE)
		val lato = createFont("Lato-Black.ttf", 50, false)
		textFont(lato)
		// Fonts are loaded lazily, so when we call text()
		// for the first time, there is significant lag.
		// This prevents it from happening during gameplay.
		text("", 0, 0)

		// This should be called last, since the game
		// clock is officially ticking at this point
		updateTimer.init()
		surface.setResizable(true)
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
