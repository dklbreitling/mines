package minesweeperlikegamepleasedontsuememichaelsoft.logic

case class Point(x: Int, y: Int) {
	def ==(rhs: Point): Boolean = this.x == rhs.x && this.y == rhs.y
}
