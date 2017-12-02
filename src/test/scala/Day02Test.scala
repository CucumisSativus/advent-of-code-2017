import org.scalatest.{MustMatchers, WordSpec}

class Day02Test extends WordSpec with MustMatchers{
  "proper input should give proper output" in {
    val input =
      """
        |5 1 9 5
        |7 5 3
        |2 4 6 8
      """.stripMargin

    Day02.solve(input) mustBe 18
  }

  "line difference is ok" in {
    val line = "5 1 9 5"
    Day02.calculateLineDifference(line) mustBe 8
  }

}
