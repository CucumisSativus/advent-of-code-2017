import org.scalatest.{MustMatchers, WordSpec}

class Day04Test extends WordSpec with MustMatchers{
  "should classify valid line" in {
    assert(Day04.isLineValid("aa bb cc dd ee"))
  }

  "should classify invalid line" in {
    assert(!Day04.isLineValid("aa bb cc dd aa"))
  }

  "should cound valid lines" in {
    val lines =
      """
        |aa bb cc dd ee
        |aa bb cc dd aa
        |aa bb cc dd aaa
      """.stripMargin

    Day04.solve(lines) mustBe 2
  }
}
