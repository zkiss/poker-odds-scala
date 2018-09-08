package zkiss.poker

import org.scalatest.{FunSuite, Matchers}
import zkiss.cards.Face.Face
import zkiss.cards.Suit.Suit
import zkiss.cards.{Card, Face, Suit}

import scala.collection.SortedSet
import scala.collection.immutable.TreeSet

class HandTest extends FunSuite with Matchers {

  test("HighCard is found") {
    val v = hand(
      (Face.A, Suit.Spades),
      (Face.Two, Suit.Hearts),
      (Face.Five, Suit.Spades),
      (Face.Ten, Suit.Spades),
      (Face.K, Suit.Spades)
    ).value.asInstanceOf[HighCard]

    v.card should be(Card(Face.A, Suit.Spades))
  }

  test("Pair is found") {
    val pair = hand(Face.Two, Face.Two, Face.Five, Face.Ten, Face.K).value.asInstanceOf[Pair]

    pair.face should be(Face.Two)
    pair.pair should be(Set(Card(Face.Two, Suit.Hearts), Card(Face.Two, Suit.Spades)))
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
    p.value < tp.value should be(true)
    val pv: Pair = p.value.asInstanceOf[Pair]
    val tpv: TwoPairs = tp.value.asInstanceOf[TwoPairs]
    pv < tpv should be(true)
  }

  test("Straight") {
    val low = hand(
      (Face.A, Suit.Spades),
      (Face.Two, Suit.Hearts),
      (Face.Three, Suit.Diamonds),
      (Face.Four, Suit.Spades),
      (Face.Five, Suit.Diamonds)
    ).value.asInstanceOf[Straight]
    val mid = hand(
      (Face.Seven, Suit.Spades),
      (Face.Eight, Suit.Hearts),
      (Face.Nine, Suit.Diamonds),
      (Face.Ten, Suit.Spades),
      (Face.J, Suit.Spades)
    ).value.asInstanceOf[Straight]
    val high = hand(
      (Face.Ten, Suit.Spades),
      (Face.J, Suit.Hearts),
      (Face.Q, Suit.Diamonds),
      (Face.K, Suit.Spades),
      (Face.A, Suit.Hearts)
    ).value.asInstanceOf[Straight]

    low.end should be(Face.Five)
    mid.end should be(Face.J)
    high.end should be(Face.A)
    low < mid should be(true)
    mid < high should be(true)
    low < high should be(true)
  }

  test("Flush < Flush") {
    val low = hand(Face.Two, Face.Three, Face.Five, Face.Six, Face.Seven).value.asInstanceOf[Flush]
    val mid = hand(Face.Two, Face.Four, Face.Five, Face.Six, Face.Seven).value.asInstanceOf[Flush]
    val high = hand(Face.Two, Face.Three, Face.Five, Face.Six, Face.K).value.asInstanceOf[Flush]

    low < mid should be(true)
    mid < high should be(true)
    low < high should be(true)
  }

  test("RoyalFlush") {
    val rf = hand(Face.Ten, Face.J, Face.Q, Face.K, Face.A).value.asInstanceOf[RoyalFlush]

    rf.straightFlush.straight.end should be(Face.A)
  }

  test("isTarget") {
    val pairK = Pair(cards(Face.K, Face.K))
    val pairsAK = TwoPairs(Pair(cards(Face.A, Face.A)), pairK)
    Pair.isTarget(pairK) should be(true)
    Pair.isTarget(pairsAK) should be(false)
    TwoPairs.isTarget(pairK) should be(false)
    TwoPairs.isTarget(pairsAK) should be(true)
  }

  def hand(faces: Face*): Hand =
    Hand(
      cards(faces: _*).to[TreeSet]
    )

  def hand(c1: (Face, Suit), c2: (Face, Suit), c3: (Face, Suit), c4: (Face, Suit), c5: (Face, Suit)) =
    Hand(
      (c1 :: c2 :: c3 :: c4 :: c5 :: Nil)
        .map(p => Card(p._1, p._2))
        .to[TreeSet]
    )

  def cards(faces: Face*): SortedSet[Card] =
    faces
      .groupBy(f => f)
      .flatMap(e => e._2.indices.map(i => Card(e._1, Suit(i))))
      .to[SortedSet]
}
