package zkiss.poker

import java.lang.reflect.ParameterizedType

import zkiss.cards.Face.Face
import zkiss.cards.Suit.Suit
import zkiss.cards.{Card, Face}
import zkiss.poker.HandValue.valueOrder

import scala.collection.SortedSet

case class Hand(cards: SortedSet[Card]) extends Ordered[Hand] {
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
    TwoPairs,
    ThreeOfAKind,
    Straight,
    Flush
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

case class Pair(pair: collection.Set[Card]) extends HandValue[Pair] {
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
  require(highPair > lowPair)

  override def compareValue(that: TwoPairs): Int = {
    TwoPairs.ordering.compare(this, that)
  }
}

object TwoPairs extends HandValueExtractor[TwoPairs] {
  // would be nice if the implicit ordering worked...
  val ordering: Ordering[TwoPairs] = Ordering.by((tp: TwoPairs) => (tp.highPair, tp.lowPair))(Ordering.Tuple2(Pair.ordering, Pair.ordering))

  override def from(hand: Hand): Option[TwoPairs] = {
    val twoPairs = hand.cards.groupBy(c => c.face)
      .filter(e => e._2.size == 2)
      .values
      .toList
      .map(p => Pair(p))
      .sorted(Pair.ordering)

    twoPairs match {
      case low :: high :: Nil => Some(TwoPairs(high, low))
      case _ => None
    }
  }
}

case class ThreeOfAKind(cards: collection.Set[Card]) extends HandValue[ThreeOfAKind] {
  require(cards.size == 3)
  require(cards.groupBy(c => c.face).size == 1)

  val face: Face = cards.head.face

  override def compareValue(that: ThreeOfAKind): Int =
    ThreeOfAKind.ordering.compare(this, that)
}

object ThreeOfAKind extends HandValueExtractor[ThreeOfAKind] {
  val ordering: Ordering[ThreeOfAKind] = Ordering.by(t => t.face)

  override def from(hand: Hand): Option[ThreeOfAKind] =
    hand.cards.groupBy(c => c.face)
      .values
      .find(f => f.size == 3)
      .map(f => ThreeOfAKind(f))
}

case class Straight(cards: SortedSet[Card]) extends HandValue[Straight] {
  require(Straight.isStraight(cards))

  val end: Card = {
    if (Straight.contiguous(cards)) cards.lastKey
    else cards.dropRight(1).last
  }

  override def compareValue(that: Straight): Int =
    Straight.ordering.compare(this, that)
}

object Straight extends HandValueExtractor[Straight] {
  val ordering: Ordering[Straight] = Ordering.by(s => s.end)

  override def from(hand: Hand): Option[Straight] =
    if (isStraight(hand.cards)) Some(Straight(hand.cards))
    else None

  private val lowAceStraightCards = List(Face.Two, Face.Three, Face.Four, Face.Five, Face.A)

  private def contiguous(cards: SortedSet[Card]) =
    cards.firstKey.face.id + 4 == cards.lastKey.face.id

  private def isStraight(cards: SortedSet[Card]) = {
    def lowAce = cards.toList.map(c => c.face) == lowAceStraightCards

    cards.groupBy(c => c.face).size == 5 && (contiguous(cards) || lowAce)
  }
}

case class Flush(cards: SortedSet[Card]) extends HandValue[Flush] {
  require(cards.size == 5)
  require(cards.groupBy(c => c.suit).size == 1)

  val suit: Suit = cards.head.suit
  val faces: List[Face] = cards.map(c => c.face).toList

  override def compareValue(that: Flush): Int =
    faces.zip(that.faces)
      .reverse
      .dropWhile(p => p._1 == p._2)
      .headOption
      .map(p => p._1.compare(p._2))
      .getOrElse(0)
}

object Flush extends HandValueExtractor[Flush] {
  override def from(hand: Hand): Option[Flush] =
    if (hand.cards.groupBy(c => c.suit).size == 1) Some(Flush(hand.cards))
    else None
}

case class FullHouse(three: ThreeOfAKind, pair: Pair) extends HandValue[FullHouse] {
  require(three.cards.intersect(pair.pair).isEmpty)

  override def compareValue(that: FullHouse): Int =
    FullHouse.order.compare(this, that)
}

object FullHouse extends HandValueExtractor[FullHouse] {
  val order: Ordering[FullHouse] = Ordering.by((fh: FullHouse) => (fh.three, fh.pair))(Ordering.Tuple2(ThreeOfAKind.ordering, Pair.ordering))

  override def from(hand: Hand): Option[FullHouse] = for {
    three <- ThreeOfAKind.from(hand)
    pair <- Pair.from(hand)
  } yield FullHouse(three, pair)
}