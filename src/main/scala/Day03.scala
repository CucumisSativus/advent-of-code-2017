import com.sun.javafx.image.impl.IntArgb.ToIntArgbPreConv

object Day03 {
  def solve(position: Int): Int = {
    val array = buildArray(position)
    distanceInArray(array)(position)
  }

  def findClosestSquare(number: Int): Int = {
    findClosestSquare(number, 1)
  }

  def findClosestSquare(number: Int, currentSize: Int): Int = {
    val i = currentSize * currentSize
    if(i >= number) currentSize
    else findClosestSquare(number, currentSize + 2)
  }

  def buildArray(number: Int): Array[Array[Int]] = {
    if(number == 1) return Array(Array(1))

    val size = findClosestSquare(number)
    println(s"Array size $size")
    val array = Array.fill(size, size)(-1)
    val center: Point = findCentralPoint(array)

    var currentArrayRing = 1
    var nextMustNavigateThroughNewRing = false
    def nextDirection(currentMoveDirection: CurrentMoveDiirection, currentPoint: Point): CurrentMoveDiirection = {
      val xDIstance = currentPoint.x - center.x
      val yDistance = currentPoint.y - center.y

      if(xDIstance == currentArrayRing && yDistance == -currentArrayRing) Left
      else if(xDIstance == -currentArrayRing && yDistance == -currentArrayRing) Down
      else if(xDIstance == -currentArrayRing && yDistance == currentArrayRing) Right
      else if(xDIstance == currentArrayRing && yDistance == currentArrayRing) Up
      else currentMoveDirection
    }

    def fillArray(x: Int, y: Int, num: Int, nextMoveDirection: CurrentMoveDiirection): Unit = {
      array(x)(y) = num
      println(s"filling array x $x y $y num $num currentMoveDirection $nextMoveDirection")

      if(num == (2 * currentArrayRing + 1) * ( 2 * currentArrayRing +1)){
        println("increasig array ring")
        currentArrayRing +=1
      }

      val diirection = if (num == (2 * currentArrayRing - 1) * ( 2 * currentArrayRing -1) +1 ){
        Up
      } else{
        nextDirection(nextMoveDirection, Point(x, y))
      }

      diirection match {
        case Right if num < size * size =>
          fillArray(x + 1, y, num + 1, diirection)
        case Left if num < size * size =>
          fillArray(x - 1, y, num + 1, diirection)
        case Up if num < size * size =>
          fillArray(x, y - 1, num + 1, diirection)
        case Down if num < size * size =>
          fillArray(x, y + 1, num + 1, diirection)
        case _ =>
      }
    }

    array(center.x)(center.y) = 1
    fillArray(center.x + 1, center.y, 2, Up)
    array.transpose
  }


  def distanceInArray(array: Array[Array[Int]])(number: Int): Int = {
    val center = findCentralPoint(array)
    val pointCoordinates = findPointCoordinatesInArray(array, number).getOrElse(throw new Exception("wtf"))

    Math.abs(center.x - pointCoordinates.x) + Math.abs(center.y - pointCoordinates.y)
  }

  private def findCentralPoint(array: Array[Array[Int]]) = {
    Point(array.length / 2, array.head.length / 2)
  }

  def findPointCoordinatesInArray(array: Array[Array[Int]], expectedValue: Int): Option[Point] = {
    for(x <- array.indices){
      for(y <- array.head.indices){
        if(array(x)(y) == expectedValue) return Some(Point(x, y))
      }
    }
    None
  }

  case class Point(x: Int, y: Int)
  sealed trait CurrentMoveDiirection
  case object Right extends CurrentMoveDiirection
  case object Left extends CurrentMoveDiirection
  case object Up extends CurrentMoveDiirection
  case object Down extends CurrentMoveDiirection

  def main(args: Array[String]): Unit = {
    println(solve(368078))
  }
}
