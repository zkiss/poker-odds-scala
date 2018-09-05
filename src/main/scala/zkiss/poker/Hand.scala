package zkiss.poker

import zkiss.cards.Card
import zkiss.cards.Face.Face
import zkiss.poker.HandValue.valueOrder

import scala.collection.immutable.TreeSet

case class Hand(cards: TreeSet[Card]) extends Ordered[Hand] {
  require(cards.size == 5)

  val value: HandValue[_] = HandValue.of(this)

  override def compare(that: Hand): Int =
    this.value.compareGeneral(that.value)
}

sealed trait HandValue[VV <: HandValue[VV]] extends Ordered[VV] {
  def compareGeneral(other: HandValue[_]): Int =
    valueOrder.compare(this, other) match {
      case 0 => compare(other.asInstanceOf[VV])
      case r => r
    }
}

sealed trait HandValueExtractor[+T <: HandValue[_]] {
  def from(hand: Hand): Option[T]

  def isTarget(value: HandValue[_]): Boolean = value.isInstanceOf[T]
}

object HandValue {
  private val HighToLow: Array[HandValueExtractor[HandValue[_]]] = Array(
    TwoPairs,
    Pair,
    HighCard
  )

  def of(hand: Hand): HandValue[_] = HighToLow.toStream
    .map(e => e.from(hand))
    .dropWhile(o => o.isEmpty)
    .map(o => o.get)
    .head

  val valueOrder: Ordering[HandValue[_]] =
    Ordering.by((hv: HandValue[_]) => idx(hv))

  private def idx(hv: HandValue[_]): Int =
    HighToLow.indexWhere(e => e.isTarget(hv)) match {
      case -1 => throw new RuntimeException("not found")
      case i => i
    }


}

case class HighCard(card: Card) extends HandValue[HighCard] {
  override def compare(that: HighCard): Int =
    this.card.face.compare(that.card.face)
}

object HighCard extends HandValueExtractor[HighCard] {
  def from(hand: Hand): Option[HighCard] = Some(HighCard(hand.cards.last))
}

case class Pair(pair: Set[Card]) extends HandValue[Pair] {
  require(pair.size == 2)
  require(pair.groupBy(c => c.face).size == 1)

  val face: Face = pair.head.face

  override def compare(that: Pair): Int =
    this.face.compare(that.face)

}

object Pair extends HandValueExtractor[Pair] {
  override def from(hand: Hand): Option[Pair] =
    hand.cards.groupBy(c => c.face)
      .find(e => e._2.size == 2)
      .map(e => Pair(e._2))
}

case class TwoPairs(highPair: Pair, lowPair: Pair) extends HandValue[TwoPairs] {
  require(highPair.face > lowPair.face)

  override def compare(that: TwoPairs): Int =
    Ordering.by((tp: TwoPairs) => (tp.highPair, tp.lowPair))
      .compare(this, that)
}

object TwoPairs extends HandValueExtractor[TwoPairs] {
  override def from(hand: Hand): Option[TwoPairs] = {
    val twoPairs = hand.cards.groupBy(c => c.face)
      .filter(e => e._2.size == 2)
      .values
      .toList
      .map(p => Pair(p))
      .sortBy(p => p.face)

    twoPairs match {
      case low :: high :: Nil => Some(TwoPairs(high, low))
      case _ => None
    }
  }
}