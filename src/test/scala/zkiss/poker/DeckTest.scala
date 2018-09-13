package zkiss.poker

import org.mockito.{ArgumentMatchers, Mockito}
import org.scalatest.mockito.MockitoSugar
import org.scalatest.{FunSuite, Matchers}
import zkiss.cards.{Card, Face, Suit}

import scala.util.Random

class DeckTest extends FunSuite with Matchers with MockitoSugar {

  private val rnd = mock[Random]

  test("can draw last card") {
    setupIndexes(0)
    val d = new Deck(Array(Card(Face.A, Suit.Hearts)), rnd)

    d.take() should be(Card(Face.A, Suit.Hearts))
  }

  test("handles card removal") {
    setupIndexes(2, 2, 1, 0)
    val d = new Deck(Array(
      Card(Face.A, Suit.Hearts),
      Card(Face.Two, Suit.Hearts),
      Card(Face.Three, Suit.Hearts),
      Card(Face.Four, Suit.Hearts),
      Card(Face.Five, Suit.Hearts)
    ), rnd)

    d.take() should be(Card(Face.Three, Suit.Hearts))
    d.take() should be(Card(Face.Four, Suit.Hearts))
    d.take() should be(Card(Face.Two, Suit.Hearts))
    d.take() should be(Card(Face.A, Suit.Hearts))
    d.take() should be(Card(Face.Five, Suit.Hearts))
  }

  test("Can take 52 cards") {
    val d = new Deck()

    (1 to 52).foreach(_ => d.take())
  }

  test("Collects all 52 cards") {
    val d = new Deck()

    (1 to 52).map(_ => d.take()) should contain allElementsOf Deck.cards
  }

  private def setupIndexes(i: Int, idxs: Int*): Unit = {
    Mockito.when(rnd.nextInt(ArgumentMatchers.anyInt())).thenReturn(i, idxs: _*)
  }

}
