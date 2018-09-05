package zkiss.cards

import zkiss.cards.Face.Face
import zkiss.cards.Suit.Suit

object Suit extends Enumeration {
  type Suit = Value
  val Hearts, Spades, Clubs, Diamonds = Value

  def shortName(s: Suit) = s match {
    case Suit.Hearts => "H"
    case Suit.Spades => "S"
    case Suit.Clubs => "C"
    case Suit.Diamonds => "D"
  }
}

object Face extends Enumeration {
  type Face = Value
  val Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, J, Q, K, A = Value

  def shortName(f: Face) = f match {
    case Face.Two => "2"
    case Face.Three => "3"
    case Face.Four => "4"
    case Face.Five => "5"
    case Face.Six => "6"
    case Face.Seven => "7"
    case Face.Eight => "8"
    case Face.Nine => "9"
    case Face.Ten => "10"
    case other => other.toString
  }
}

case class Card(suit: Suit, face: Face) extends Ordered[Card] {
  override def compare(that: Card): Int =
    Ordering.by((c: Card) => (c.face, c.suit))
      .compare(this, that)

  override def toString: String = {
    s"${Face.shortName(face)}${Suit.shortName(suit)}"
  }
}


