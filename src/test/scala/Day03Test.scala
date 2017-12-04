import org.scalatest.{MustMatchers, WordSpec}

class Day03Test extends WordSpec with MustMatchers{
  "data 01" in {
    Day03.solve(1) mustBe 0
  }

  "data 12" in {
    Day03.solve(12) mustBe 3
  }

  "data 23" in {
    Day03.solve(23) mustBe 2
  }

  "data 1024" in {
    Day03.solve(1024) mustBe 31
  }

  "find array size" in {
    Day03.findClosestSquare(1) mustBe 1
    Day03.findClosestSquare(12) mustBe 5
    Day03.findClosestSquare(23) mustBe 5
  }
  "build array" in {
    val array: Array[Array[Int]] = Array(
      Array(17,  16,  15,  14,  13),
      Array(18,   5,   4,   3,  12),
      Array(19,   6,   1,   2,  11),
      Array(20,   7,   8,   9,  10),
      Array(21,  22,  23,  24,  25)
    )

    Day03.buildArray(23) mustBe array
  }

  "build small array" in {
    val array: Array[Array[Int]] = Array(
      Array(5,   4,   3),
      Array(6,   1,   2),
      Array(7,   8,   9)
    )

    Day03.buildArray(8) mustBe array
  }

  "build biggest array" in {
    val array: Array[Array[Int]] = Array(
      Array(37, 36,  35,  34,  33,  32, 31),
      Array(38, 17,  16,  15,  14,  13, 30),
      Array(39, 18,   5,   4,   3,  12, 29),
      Array(40, 19,   6,   1,   2,  11, 28),
      Array(41, 20,   7,   8,   9,  10, 27),
      Array(42, 21,  22,  23,  24,  25, 26),
      Array(43, 44,  45,  46,  47,  48, 49)
    )

    Day03.buildArray(30) mustBe array
  }
  "distance in array" in {
    val array: Array[Array[Int]] = Array(
      Array(17,  16,  15,  14,  13),
      Array(18,   5,   4,   3,  12),
      Array(19,   6,   1,   2,  11),
      Array(20,   7,   8,   9,  10),
      Array(21,  22,  23,  24,  25)
    )

    val dist = Day03.distanceInArray(array) _

    dist(1) mustBe 0
    dist(12) mustBe 3
    dist(23) mustBe 2
  }
}
