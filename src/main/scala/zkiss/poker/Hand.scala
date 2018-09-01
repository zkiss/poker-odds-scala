package zkiss.poker

import zkiss.cards.Card
import zkiss.cards.Face.Face

import scala.collection.immutable.TreeSet

case class Hand(cards: TreeSet[Card]) extends Ordered[Hand] {
  require(cards.size == 5)

  val value: HandValue = HandValue.of(this)

  override def compare(that: Hand): Int =
    this.value.compare(that.value)
}

sealed trait HandValue extends Ordered[HandValue]

sealed trait HandValueExtractor[+T] {
  def from(hand: Hand): Option[T]

  def isTarget(value: HandValue): Boolean = value.isInstanceOf[T]
}

object HandValue {
  private val Values: Array[HandValueExtractor[HandValue]] = Array(
    TwoPairs,
    Pair,
    HighCard
  )

  def of(hand: Hand): HandValue = Values.toStream
    .map(e => e.from(hand))
    .dropWhile(o => o.isEmpty)
    .map(o => o.get)
    .head

  val byType: Ordering[HandValue] =
    Ordering.by(hv => Values.indexWhere(e => e.isTarget(hv)))

}

case class HighCard(card: Card) extends HandValue {
  override def compare(that: HandValue): Int =
    HandValue.byType.thenComparing(HighCard.ordering)
}

object HighCard extends HandValueExtractor[HighCard] {
  private val ordering =
    Ordering.by((hc:HighCard)=>hc.card.face)
  private val comparator =
    HandValue.byType
      .thenComparing((hv: HandValue) => hv.asInstanceOf[HighCard].card.face)

  def from(hand: Hand): Option[HighCard] = Some(HighCard(hand.cards.last))
}

case class Pair(pair: Set[Card]) extends HandValue {
  require(pair.size == 2)
  require(pair.groupBy(c => c.face).size == 1)

  val face: Face = pair.head.face

  override def compare(that: HandValue): Int =
    Pair.comparator.compare(this, that)

}

object Pair extends HandValueExtractor[Pair] {
  private val comparator =
    HandValue.byType
      .thenComparing((hv: HandValue) => hv.asInstanceOf[Pair].face)

  override def from(hand: Hand): Option[Pair] =
    hand.cards.groupBy(c => c.face)
      .find(e => e._2.size == 2)
      .map(e => Pair(e._2))
}

case class TwoPairs(highPair: Pair, lowPair: Pair) extends HandValue {
  require(highPair.face > lowPair.face)

  override def compare(that: HandValue): Int = TwoPairs.comparator.compare(this, that)
}

object TwoPairs extends HandValueExtractor[TwoPairs] {
  private val comparator =
    HandValue.byType
      .thenComparing(hv => hv.asInstanceOf[TwoPairs].highPair)
      .thenComparing(hv => hv.asInstanceOf[TwoPairs].lowPair)

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