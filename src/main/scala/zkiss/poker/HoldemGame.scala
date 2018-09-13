package zkiss.poker

import zkiss.cards.Card

import scala.collection.SortedSet

class HoldemGame(
  private val cards: Set[Card],
  private val players: Int,
  private val dealt: Set[Card] = Set(),
  private val drawOk: Boolean = true,
  private val deck: Deck = new Deck()
) {
  require(cards.size == 2)
  require(players > 1)
  require(players <= 22)
  require(dealt.size <= 5)

  def finish(): Boolean = {
    cards.foreach(c => deck.remove(c))
    dealt.foreach(c => deck.remove(c))

    val allDealt =
      dealt ++
        (0 to (5 - dealt.size))
          .map(_ => deck.take())

    val hand = bestHand(cards ++ allDealt)
    val strongestOpponent = (1 to players)
      .map(_ => allDealt + deck.take() + deck.take())
      .map(c => bestHand(c))
      .max

    hand > strongestOpponent ||
      (drawOk && hand >= strongestOpponent)
  }

  private def bestHand(cards: Set[Card]) = {
    cards.subsets(5)
      .map(s => Hand(s.to[SortedSet]))
      .max
  }
}
