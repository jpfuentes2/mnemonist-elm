module PlayingCards where

type Suit = Hearts | Spades | Clubs | Diamonds

--type Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine |
  --Ten | Jack | Queen | King | Ace

type alias Rank = String

type alias Card = (Suit, Rank)

type alias Deck = List Card

suits : List Suit
suits = [Hearts, Spades, Clubs, Diamonds]

rank : List Rank
rank = ["2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K", "A"]

--replicate : Int -> a -> List a
--replicate n a =
  --if n <= 0 then [] else a :: (replicate (n-1) a)

-- no applicative :(
makeDeck : Deck
makeDeck = List.concatMap (\v -> List.map (\s -> (s, v)) suits) rank

suitSymbol : Suit -> String
suitSymbol suit =
  case suit of
    Hearts -> "♥"
    Spades -> "♠"
    Clubs -> "♣"
    Diamonds -> "♦"
