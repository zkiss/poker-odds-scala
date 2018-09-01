package zkiss.cards

import zkiss.cards.Face.Face
import zkiss.cards.Suit.Suit

object Suit extends Enumeration {
  type Suit = Value
  val Hearts, Spades, Clubs, Diamonds = Value
}

object Face extends Enumeration {
  type Face = Value
  val Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, J, Q, K, A = Value
}

case class Card(suit: Suit, face: Face) extends Ordered[Card] {
  override def compare(that: Card): Int =
    this.face.compare(that.face)

  override def toString: String = {
    val f = face match {
      case Face.Two => "2"
      case Face.Three => "3"
      case Face.Four => "4"
      case Face.Five => "5"
      case Face.Six => "6"
      case Face.Seven => "7"
      case Face.Eight => "8"
      case Face.Nine => "9"
      case Face.Ten => "10"
      case ff => ff.toString
    }
    val s = suit match {
      case Suit.Hearts => "H"
      case Suit.Spades => "S"
      case Suit.Clubs => "C"
      case Suit.Diamonds => "D"
    }
    s"$f$s"
  }
}


