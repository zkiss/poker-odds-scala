package zkiss.poker

import org.scalatest.{FunSuite, Matchers}
import zkiss.cards.Face.Face
import zkiss.cards.{Card, Face, Suit}

import scala.collection.immutable.TreeSet

class HandTest extends FunSuite with Matchers {

  test("HighCard is found") {
    val v = hand(Face.Two, Face.Three, Face.Five, Face.Ten, Face.A).value

    v.isInstanceOf[HighCard] should be(true)
  }

  def hand(faces: Face*): Hand = Hand(TreeSet(faces.map(f => Card(Suit.Clubs, f)): _*))
}
