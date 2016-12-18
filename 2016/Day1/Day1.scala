trait Way
case object Left extends Way 
case object Right extends Way

object Way {
	def apply(w : Char):Way = w match {
		case 'L' => Left
		case 'R' => Right
	}
}

trait Compass
case object North extends Compass
case object South extends Compass
case object East extends Compass
case object West extends Compass

case class Coordinate(x:Integer, y:Integer)

case class Direction(way:Way, steps:Integer)

case class Position(coord:Coordinate, compass:Compass)

def changeDirection(direction:Direction, 
					coordinate:Coordinate, 
					compass:Compass) : Position = {
	val (x, y) = (coordinate.x, coordinate.y)
	val (way, steps) = (direction.way, direction.steps)
	
	(compass, way) match {
		case (North, Left) => Position(Coordinate(x - steps, y), West)
		case (North, Right) => Position(Coordinate(x + steps, y), East)
		case (South, Left) => Position(Coordinate(x + steps, y), East)
		case (South, Right) => Position(Coordinate(x - steps, y), West)
		case (East, Left) => Position(Coordinate(x, y + steps), North)
		case (East, Right) => Position(Coordinate(x, y - steps), South)
		case (West, Left) => Position(Coordinate(x, y - steps), South)
		case (West, Right) => Position(Coordinate(x, y + steps), North)
	}
}

def getDestinationCoordinate(directions: List[Direction]): Coordinate = {
	val start = Position(Coordinate(0,0), North)
	val finalPosition = directions.foldLeft(start){
			(position, direction) => changeDirection(direction, position.coord, position.compass)
		}
	finalPosition.coord
}

def parseDirection(token: String):Direction = {
	val (w, s) = (token.head, token.tail)
	val way = Way(w)
	val steps = Integer.parseInt(s)
	Direction(way, steps)
}

def adventInput(input: String):List[Direction] = {
	//val input = scala.io.Source.fromURL(inputUrl).mkString
	input.split(",").toList.map(r => parseDirection(r.trim))
}

def distance(input: String):Integer = {
	val directions = adventInput(input)
	val finalCoordinate = getDestinationCoordinate(directions)
	val (finalX, finalY) = (finalCoordinate.x, finalCoordinate.y)
	Math.abs(finalX) + Math.abs(finalY)
}
