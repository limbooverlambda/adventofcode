object Day2 {
    class Position private (val x: Int, val y:Int){
      override def equals(other: Any):Boolean = other match {
        case that: Position =>
          (that canEqual this) &&
          (x == that.x) &&
          (y == that.y)
        case _ => false
      }

      def canEqual(other: Any):Boolean = other.isInstanceOf[Position]

      override def hashCode:Int = 41*(41*x) + y
    }
    object Position{
      val MaxVal = 1
      val MinVal = -1
      private def process(i: Int):Int = i match {
        case a if(a > MaxVal) => MaxVal
        case a if(a < MinVal) => MinVal
        case _ => i
      }
      def apply(x:Int, y:Int):Position = {
        println("x:"+x+"y:"+y)
        new Position(process(x),process(y))
      }
    }

    val posVal:Map[Position, Int] =
        Map(Position(0,0) -> 5,
            Position(0,1) -> 2,
            Position(0,-1) -> 8,
            Position(-1, 0) -> 4,
            Position(1, 0) -> 6,
            Position(1, 1) -> 3,
            Position(-1, -1) -> 7,
            Position(-1, 1) -> 1,
            Position(1, -1) -> 9)

    def processLine(position:Position)(line : String):Position = {
     line.foldLeft(position){
      (p, token) => {
        nextPosition(p,token)
      }
     }
    }

    def nextPosition(position: Position, token: Char):Position={
      val (x,y) = (position.x, position.y)
      token match {
        case 'U' => Position(x, y + 1)
        case 'D' => Position(x, y - 1)
        case 'L' => Position(x - 1, y)
        case 'R' => Position(x + 1, y)
      }
    }

    def runner(file: String):Unit = {
      val lb = scala.io.Source.fromFile(file).getLines.foldLeft((0, Position(0,0))){
        (valAndPos, line) => {
          val (v, pos) = valAndPos
          val position = processLine(pos)(line)
          val posValue = posVal.get(position).get
          (if (v == 0) posValue else (v*10 + posValue), position)
        }
      }
      println("The code is:"+lb._1)
    }
}
