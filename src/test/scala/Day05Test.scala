import org.scalatest.{MustMatchers, WordSpec}

class Day05Test extends WordSpec with MustMatchers{
  "part1 should return proper jump numbers" in {
    val instrustions = """0
                         |3
                         |0
                         |1
                         |-3
                         |""".stripMargin
    Day05.solve(instrustions) mustBe 5
  }

  "part2 should return proper jump numbers" in {
    val instrustions = """0
                         |3
                         |0
                         |1
                         |-3
                         |""".stripMargin
    Day05.solvePart2(instrustions) mustBe 10
  }

}
