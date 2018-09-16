package zkiss.oddscalc

import org.scalatest.{FunSuite, Matchers}
import zkiss.cards.{Card, Face, Suit}

class OddsCalcTest extends FunSuite with Matchers {

  test("runs sim") {
    val res = OddsCalc.simulate(
      Set(Card(Face.A, Suit.Spades), Card(Face.A, Suit.Diamonds)),
      4,
      Set(),
      10000
    )

    res._1 + res._2 + res._3 should be(10000)
  }

}
