package zkiss.poker

import zkiss.cards.{Card, Face, Suit}

import scala.util.Random

object Deck {
  val cards: Array[Card] = (for {
    suit <- Suit.values
    face <- Face.values
  } yield Card(face, suit))
    .toArray

}

class Deck(
  private val cards: Array[Card] = Deck.cards,
  private val rnd: Random = Random
) {
  private var size = cards.length

  def take(): Card = {
    val nextIdx = rnd.nextInt(size)
    remove(nextIdx)
  }

  def remove(card: Card): Unit = {
    remove(cards.indexOf(card))
  }

  private def remove(idx: Int): Card = {
    require(idx >= 0)
    require(idx < size)
    val ret = cards(idx)
    size = size - 1
    Array.copy(cards, idx + 1, cards, idx, size - idx)
    ret
  }
}
