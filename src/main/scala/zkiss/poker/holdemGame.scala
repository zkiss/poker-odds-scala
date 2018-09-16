package zkiss.poker

import zkiss.cards.Card

import scala.collection.SortedSet

object HoldemGame {
  def start(
    cards: Set[Card],
    players: Int,
    dealt: Set[Card] = Set(),
    deck: Deck = new Deck()
  ): HoldemGame = {
    val p1 = Player(cards)

    cards.foreach(c => deck.remove(c))
    dealt.foreach(c => deck.remove(c))

    val opponents = (1 to players).map(_ => Player(Set(deck.take(), deck.take())))
    new HoldemGame(
      opponents.toSet + p1,
      dealt,
      deck
    )
  }
}

class HoldemGame(
  private val players: Set[Player],
  private val dealt: Set[Card] = Set(),
  private val deck: Deck
) {
  require(players.size > 1)
  require(players.size <= 22)
  require(dealt.size <= 5)

  def finish(): HoldemGameResult = {
    val allDealt =
      dealt ++
        (0 until (5 - dealt.size))
          .map(_ => deck.take())

    HoldemGameResult(players, allDealt)
  }
}

case class Player(cards: Set[Card]) {
  require(cards.size == 2)

  def hand(dealt: Set[Card]): Hand = {
    require(dealt.size == 5)

    (dealt ++ cards).subsets(5)
      .map(s => Hand(s.to[SortedSet]))
      .max
  }
}


case class HoldemGameResult(
  players: Set[Player],
  dealt: Set[Card]
) {
  require(players.nonEmpty)
  require(dealt.size == 5)

  type Winner = (Player, Hand)

  def winners: Set[Winner] = {
    val sorted = players.map(p => (p, p.hand(dealt)): Winner)
      .toList
      .sortWith((w1: Winner, w2: Winner) => w2._2 < w1._2)

    sorted.takeWhile(w => w._2.sameAs(sorted.head._2))
      .toSet
  }
}