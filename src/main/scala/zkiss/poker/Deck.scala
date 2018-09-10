package zkiss.poker

import zkiss.cards.{Card, Face, Suit}

object Deck {
  def cards = for {
    suit <- Suit.values
    face <- Face.values
  } yield Card(face, suit)

  def shuffle: Deck =
    Deck(
      util.Random.shuffle(cards.toList)
    )
}

case class Deck(cards: List[Card]) {
  def take: (Card, Deck) =
    (cards.head, Deck(cards.tail))
}
