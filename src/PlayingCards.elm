module PlayingCards where

import List

type Suit = Hearts | Spades | Clubs | Diamonds

type Rank = Two
          | Three
          | Four
          | Five
          | Six
          | Seven
          | Eight
          | Nine
          | Ten
          | Jack
          | Queen
          | King
          | Ace

type alias Card = (Suit, Rank)

type alias Cards = List Card

suits : List Suit
suits = [Hearts, Spades, Clubs, Diamonds]

ranks : List (Rank, String)
ranks = [
        (Two,"2"),(Three,"3"),(Four,"4"),(Five,"5"),
        (Six,"6"),(Seven,"7"),(Eight,"8"),
        (Nine,"9"),(Ten,"10"),(Jack,"J"),
        (Queen,"Q"),(King,"K"),(Ace,"A")
        ]

showRank : Rank -> String
showRank rank =
  List.filter (fst >> (==) rank) ranks
  |> List.head
  |> Maybe.map snd
  |> Maybe.withDefault "UNKNOWN"

--replicate : Int -> a -> List a
--replicate n a =
  --if n <= 0 then [] else a :: (replicate (n-1) a)

-- no applicative :(
makeDeck : Cards
makeDeck = List.concatMap (\v -> List.map (\s -> (s, v)) suits) <| List.map fst ranks

suitSymbol : Suit -> String
suitSymbol suit =
  case suit of
    Hearts -> "♥"
    Spades -> "♠"
    Clubs -> "♣"
    Diamonds -> "♦"
