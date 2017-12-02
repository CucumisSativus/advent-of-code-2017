import org.scalatest.{MustMatchers, WordSpec}

class Day01Test extends WordSpec with MustMatchers{

  "solve 1122" in {
    Day01.solve("1122") mustBe 3
  }

  "solve 1111" in {
    Day01.solve("1111") mustBe 4
  }

  "solve 1234" in {
    Day01.solve("1234") mustBe 0
  }

  "solve 91212129" in {
    Day01.solve("91212129") mustBe 9
  }
}
