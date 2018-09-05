package zkiss.cards

import org.scalatest.{FunSuite, Matchers}

class FaceTest extends FunSuite with Matchers {

  test("comparison works as expected") {
    Face.A > Face.Two should be (true)
    Face.A > Face.K should be (true)
    Face.K > Face.Ten should be (true)
    Face.J == Face.J should be (true)
    Face.Nine > Face.K should be (false)
  }

  test("shortName") {
    Face.shortName(Face.A) should be ("A")
    Face.shortName(Face.Ten) should be ("10")
  }

}
