import org.scalatest.{MustMatchers, WordSpec}

class Day05Test extends WordSpec with MustMatchers{
  "should return proper jump numbers" in {
    val instrustions = """0
                         |3
                         |0
                         |1
                         |-3
                         |""".stripMargin
    Day04.solve(instrustions) mustBe 5
  }
}
