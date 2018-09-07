package zkiss.poker

import org.scalatest.{FunSuite, Matchers}
import zkiss.cards.Face.Face
import zkiss.cards.{Card, Face, Suit}

import scala.collection.immutable.TreeSet

class HandTest extends FunSuite with Matchers {

  test("HighCard is found") {
    val v = hand(Face.A, Face.Two, Face.Five, Face.Ten, Face.K).value.asInstanceOf[HighCard]

    v.card should be(cards(Face.A).head)
  }

  test("Pair is found") {
    val pair = hand(Face.Two, Face.Two, Face.Five, Face.Ten, Face.K).value.asInstanceOf[Pair]

    pair.face should be(Face.Two)
    pair.pair should be(Set(Card(Suit.Hearts, Face.Two), Card(Suit.Spades, Face.Two)))
  }

  test("TwoPairs is found") {
    val v = hand(Face.Two, Face.Two, Face.Three, Face.Three, Face.K).value

    v.isInstanceOf[TwoPairs] should be(true)
    val tp = v.asInstanceOf[TwoPairs]
    tp.lowPair should be(Pair(cards(Face.Two, Face.Two)))
    tp.highPair should be(Pair(cards(Face.Three, Face.Three)))
  }

  test("Pair < TwoPairs") {
    val p = hand(Face.K, Face.Two, Face.Three, Face.Four, Face.K)
    val tp = hand(Face.Two, Face.Two, Face.Three, Face.Three, Face.A)

    p < tp should be(true)
  }

  test("isTarget") {
    val pairK = Pair(cards(Face.K, Face.K))
    val pairsAK = TwoPairs(Pair(cards(Face.A, Face.A)), pairK)
    Pair.isTarget(pairK) should be(true)
    Pair.isTarget(pairsAK) should be(false)
    TwoPairs.isTarget(pairK) should be(false)
    TwoPairs.isTarget(pairsAK) should be(true)
  }

  def hand(faces: Face*): Hand = Hand(
    cards(faces: _*).to[TreeSet]
  )

  def cards(faces: Face*): Set[Card] = faces
    .groupBy(f => f)
    .flatMap(e => e._2.indices.map(i => Card(Suit(i), e._1)))
    .toSet
}
