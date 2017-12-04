import com.sun.javafx.image.impl.IntArgb.ToIntArgbPreConv

object Day03 {
  def solve(position: Int): Int = {
    if(position == 1) 0
    else {
      val array = buildArray(position)
      distanceInArray(array)(position)
    }
  }

  def findClosestSquare(number: Int): Int = {
    findClosestSquare(number, 1)
  }

  def findClosestSquare(number: Int, currentSize: Int): Int = {
    val i = currentSize * currentSize
    if (i >= number) currentSize
    else findClosestSquare(number, currentSize + 2)
  }

  def buildArray(number: Int): Array[Array[Int]] = {
    val arraySize = findClosestSquare(number)
    println(s"Array size $arraySize")
    val array = Array.fill(arraySize, arraySize)(-1)
    val center: Point = findCentralPoint(array)

    var currentArrayRing = 1

    def fillArray(currentPoint: Point, currentNumber: Int, currentMovingDirection: CurrentMoveDiirection): Unit = {
      val justEneteredNewRing = currentNumber == (2 * currentArrayRing - 1) * (2 * currentArrayRing - 1) + 1
      val approachedLastNumberInRing = currentNumber == (2 * currentArrayRing + 1) * (2 * currentArrayRing + 1)
      array(currentPoint.x)(currentPoint.y) = currentNumber
      println(s"filling array x ${currentPoint.x} y ${currentPoint.y} num $currentNumber currentMoveDirection $currentMovingDirection")

      if (approachedLastNumberInRing) {
        println("increasig array ring")
        currentArrayRing += 1
      }

      val direction = if (justEneteredNewRing) {
        Up
      } else {
        nextDirection(currentArrayRing, center, currentMovingDirection, currentPoint)
      }

      if(currentNumber < arraySize * arraySize){
        fillArray(currentPoint.move(direction), currentNumber + 1, direction)
      }
    }

    array(center.x)(center.y) = 1
    fillArray(Point(center.x + 1, center.y), 2, Up)
    array.transpose
  }

  def nextDirection(currentArrayRing: Int, center: Point, currentMoveDirection: CurrentMoveDiirection, currentPoint: Point): CurrentMoveDiirection = {
    val currentPositionInRing = evaluateCurrentPosition(currentArrayRing, center, currentPoint)
    currentPositionInRing match {
      case TopRightCorner => Left
      case TopLeftCorner => Down
      case BottomLeftCorner => Right
      case BottomRightCorner => Up
      case Other => currentMoveDirection
    }
  }

  def evaluateCurrentPosition(currentArrayRing: Int, center: Point, currentPoint: Point): CurrentPositionInRing = {
    val xDIstance = currentPoint.x - center.x
    val yDistance = currentPoint.y - center.y

    val isAtTopRightCorner = xDIstance == currentArrayRing && yDistance == -currentArrayRing
    val isAtTopLeftCorner = xDIstance == -currentArrayRing && yDistance == -currentArrayRing
    val isAtBottomLeftCornet = xDIstance == -currentArrayRing && yDistance == currentArrayRing
    val isAtBottomRightCorner = xDIstance == currentArrayRing && yDistance == currentArrayRing

    if (isAtTopRightCorner) TopRightCorner
    else if (isAtTopLeftCorner) TopLeftCorner
    else if (isAtBottomLeftCornet) BottomLeftCorner
    else if (isAtBottomRightCorner) BottomRightCorner
    else Other
  }

  sealed trait CurrentPositionInRing
  case object TopRightCorner extends CurrentPositionInRing
  case object TopLeftCorner extends CurrentPositionInRing
  case object BottomLeftCorner extends CurrentPositionInRing
  case object BottomRightCorner extends CurrentPositionInRing
  case object Other extends CurrentPositionInRing

  def distanceInArray(array: Array[Array[Int]])(number: Int): Int = {
    val center = findCentralPoint(array)
    val pointCoordinates = findPointCoordinatesInArray(array, number).getOrElse(throw new Exception("wtf"))

    Math.abs(center.x - pointCoordinates.x) + Math.abs(center.y - pointCoordinates.y)
  }

  private def findCentralPoint(array: Array[Array[Int]]) = {
    Point(array.length / 2, array.head.length / 2)
  }

  def findPointCoordinatesInArray(array: Array[Array[Int]], expectedValue: Int): Option[Point] = {
    for (x <- array.indices) {
      for (y <- array.head.indices) {
        if (array(x)(y) == expectedValue) return Some(Point(x, y))
      }
    }
    None
  }

  case class Point(x: Int, y: Int){
    def move(direction: CurrentMoveDiirection): Point = direction match {
      case Right => copy(x = x+1)
      case Left => copy(x = x - 1)
      case Up => copy(y = y -1)
      case Down => copy(y = y +1)
    }
  }



  sealed trait CurrentMoveDiirection

  case object Right extends CurrentMoveDiirection

  case object Left extends CurrentMoveDiirection

  case object Up extends CurrentMoveDiirection

  case object Down extends CurrentMoveDiirection


  def main(args: Array[String]): Unit = {
    println(solve(368078))
  }
}
