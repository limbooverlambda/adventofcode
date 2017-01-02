object Day3{
  case object Triangle{
    def isTriangle(input: String):Boolean = {
      val i:Array[Int] = input.trim.split(" +").map(Integer.parseInt)
      val (a, b, c) = (i(0), i(1), i(2))
      List((a + b) > c, (b + c) > a, (a + c) > b).reduceLeft(_ && _)
    }
  }

  def runner(input:String):Int = {
    scala.io.Source.fromFile(input).getLines().foldLeft(0){
      (num, line) => {
        println(line)
        if(Triangle.isTriangle(line)) num + 1 else num
      }
    }
  }
}
