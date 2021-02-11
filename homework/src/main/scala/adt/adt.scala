package adt

object adt {

  sealed trait CardRank
  object CardRank {
    final case object Ace   extends CardRank
    final case object King  extends CardRank
    final case object Queen extends CardRank
    final case object Jack  extends CardRank
    final case object Ten   extends CardRank
    final case object Nine  extends CardRank
    final case object Eight extends CardRank
    final case object Seven extends CardRank
    final case object Six   extends CardRank
    final case object Five  extends CardRank
    final case object Four  extends CardRank
    final case object Three extends CardRank
    final case object Two   extends CardRank
  }

  sealed trait CardSuit
  object CardSuit {
    final case object Clubs    extends CardSuit
    final case object Diamonds extends CardSuit
    final case object Hearts   extends CardSuit
    final case object Spades   extends CardSuit
  }

  final case class Card(rank: CardRank, suit: CardSuit)

  sealed trait Hand
  object Hand {
    final case class TexasHand(card1: Card, card2: Card)                           extends Hand
    final case class OmahaHand(card1: Card, card2: Card, card3: Card, card4: Card) extends Hand
  }

  final case class Board(card1: Card, card2: Card, card3: Card, card4: Card, card5: Card)

  sealed trait PokerCombination
  object PokerCombination {
    final case object StraightFlush extends PokerCombination
    final case object FourKind      extends PokerCombination
    final case object FullHouse     extends PokerCombination
    final case object Flush         extends PokerCombination
    final case object Straight      extends PokerCombination
    final case object ThreeKind     extends PokerCombination
    final case object TwoPairs      extends PokerCombination
    final case object Pair          extends PokerCombination
    final case object HighCard      extends PokerCombination
  }

  final case class TestResult(hand: Hand, pokerCombination: PokerCombination)
  final class TestCase(board: Board, hands: List[Hand]) {
    def result(board: Board, hands: List[Hand]): List[TestResult] = ???
  }

}
