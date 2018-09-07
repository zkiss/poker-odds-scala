package zkiss.poker

import java.lang.reflect.ParameterizedType

import zkiss.cards.Card
import zkiss.cards.Face.Face
import zkiss.poker.HandValue.valueOrder

import scala.collection.immutable.TreeSet

case class Hand(cards: TreeSet[Card]) extends Ordered[Hand] {
  require(cards.size == 5)

  val value: HandValue[_] = HandValue.of(this)

  override def compare(that: Hand): Int =
    this.value.compare(that.value)
}

sealed trait HandValue[V <: HandValue[V]] extends Ordered[HandValue[_]] {
  final override def compare(that: HandValue[_]): Int =
    compareGeneral(that)

  final def compareGeneral(that: HandValue[_]): Int =
    valueOrder.compare(this, that) match {
      case 0 => compareValue(that.asInstanceOf[V]) // must be same type
      case r => r
    }

  def compareValue(that: V): Int
}

sealed trait HandValueExtractor[+T <: HandValue[_]] {
  private val typeClass = getClass.getGenericInterfaces
    .filter(t => t.isInstanceOf[ParameterizedType])
    .map(t => t.asInstanceOf[ParameterizedType])
    .find(t => t.getRawType == Class.forName("zkiss.poker.HandValueExtractor"))
    .get
    .getActualTypeArguments.head
    .asInstanceOf[Class[_]]

  def from(hand: Hand): Option[T]

  def isTarget(value: HandValue[_]): Boolean =
    typeClass.isAssignableFrom(value.getClass)
}

object HandValue {
  private val strengthOrder: Array[HandValueExtractor[HandValue[_]]] = Array(
    HighCard,
    Pair,
    TwoPairs
  )

  def of(hand: Hand): HandValue[_] = strengthOrder
    .toStream // stream for lazy eval
    .reverse
    .map(e => e.from(hand))
    .dropWhile(o => o.isEmpty)
    .map(o => o.get)
    .head

  val valueOrder: Ordering[HandValue[_]] =
    Ordering.by((hv: HandValue[_]) => strengthIdx(hv))

  private def strengthIdx(hv: HandValue[_]): Int =
    strengthOrder.indexWhere(e => e.isTarget(hv)) match {
      case -1 => throw new RuntimeException(s"HandValue missing from strength order: ${hv.getClass}")
      case i => i
    }
}

case class HighCard(card: Card) extends HandValue[HighCard] {
  override def compareValue(that: HighCard): Int =
    this.card.face.compare(that.card.face)
}

object HighCard extends HandValueExtractor[HighCard] {
  def from(hand: Hand): Option[HighCard] =
    Some(HighCard(hand.cards.lastKey))
}

case class Pair(pair: Set[Card]) extends HandValue[Pair] {
  require(pair.size == 2)
  require(pair.groupBy(c => c.face).size == 1)

  val face: Face = pair.head.face

  override def compareValue(that: Pair): Int =
    Pair.ordering.compare(this, that)

}

object Pair extends HandValueExtractor[Pair] {
  override def from(hand: Hand): Option[Pair] =
    hand.cards.groupBy(c => c.face)
      .find(e => e._2.size == 2)
      .map(e => Pair(e._2))

  val ordering: Ordering[Pair] = Ordering.by((p: Pair) => p.face)
}

case class TwoPairs(highPair: Pair, lowPair: Pair) extends HandValue[TwoPairs] {
  require(highPair.face > lowPair.face)

  override def compareValue(that: TwoPairs): Int = {
    TwoPairs.ordering.compare(this, that)
  }
}

object TwoPairs extends HandValueExtractor[TwoPairs] {
  val ordering: Ordering[TwoPairs] = Ordering.by((tp: TwoPairs) => (tp.highPair, tp.lowPair))(Ordering.Tuple2(Pair.ordering, Pair.ordering))

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