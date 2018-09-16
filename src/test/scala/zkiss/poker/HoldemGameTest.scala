package zkiss.poker

import org.scalatest.mockito.MockitoSugar
import org.scalatest.{FunSuite, Matchers}
import zkiss.cards.{Card, Face, Suit}

class HoldemGameTest extends FunSuite with Matchers with MockitoSugar {

  test("GameResult") {
    val ks = Player(Set(Card(Face.K, Suit.Hearts), Card(Face.K, Suit.Diamonds)))
    val fives = Player(Set(Card(Face.Five, Suit.Hearts), Card(Face.Five, Suit.Diamonds)))

    val winners = HoldemGameResult(Set(ks, fives), Set(
      Card(Face.Five, Suit.Clubs),
      Card(Face.K, Suit.Clubs),
      Card(Face.Five, Suit.Spades),
      Card(Face.A, Suit.Spades),
      Card(Face.Q, Suit.Diamonds)
    )).winners

    winners should have size 1
    winners.head._1 == fives should be(true)
  }

  test("can play game") {
    val result = HoldemGame.start(
      Set(Card(Face.A, Suit.Diamonds), Card(Face.A, Suit.Spades)),
      4
    ).finish()

    result.winners should not be empty
  }

}
