package zkiss.oddscalc

import zkiss.cards.Card
import zkiss.poker.HoldemGame

object OddsCalc {

  type SimulationResult = (Int, Int, Int)

  def simulate(
    cards: Set[Card],
    players: Int,
    dealt: Set[Card] = Set(),
    games: Int
  ): SimulationResult =
    (0 until games).toStream // for lazyness
      .map(_ => HoldemGame.start(cards, players, dealt).finish())
      .map(r => {
        val winners = r.winners
        if (winners.size == 1 && winners.head._1.cards == cards) (1, 0, 0)
        else if (winners.count(w => w._1.cards == cards) == 1) (0, 1, 0)
        else (0, 0, 1)
      })
      .reduce((a1, a2) => (a1._1 + a2._1, a1._2 + a2._2, a1._3 + a2._3))

}
