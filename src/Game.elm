import String
import List exposing (head, drop)
import Set
import Time exposing (every, second)
import Html exposing (..)
import Html.Attributes exposing (class)
import Keyboard
import Signal
import Graphics.Element exposing (middle, container, Element)
import Text
import Maybe

type Control = Next | Back | NoOp

type alias Model =
  { prevCards: Deck
  , nextCards : Deck
  , card : Card
  , control : Control
  }

type alias Keys = { x: Int, y: Int }

type Suit = Hearts | Spades | Clubs | Diamonds

--type Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine |
  --Ten | Jack | Queen | King | Ace

type alias Rank = String

type alias Card = (Suit, Rank)

type alias Deck = List Card

suits : List Suit
suits = [Hearts, Spades, Clubs, Diamonds]

values : List Rank
values = ["2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K", "A"]

replicate : Int -> a -> List a
replicate n a =
  if n <= 0 then [] else a :: (replicate (n-1) a)

-- no applicative :(
makeDeck : Deck
makeDeck = List.concatMap (\v -> List.map (\s -> (s, v)) suits) values

suitSymbol : Suit -> String
suitSymbol suit =
  case suit of
    Hearts -> "♥"
    Spades -> "♠"
    Clubs -> "♣"
    Diamonds -> "♦"

showCard : Card -> Html.Html
showCard card =
  let (suit, value) = card
      symbol = suitSymbol suit
      suitName = suit |> toString |> String.toLower
  in
    div [class ("card " ++ suitName)]
      [ div [class "corner top"]
        [ span [class "number"] [text value]
        , span [] [text symbol]
        ]
      , span [class "suit top_center"] [text symbol]
      , span [class "suit bottom_center"] [text symbol]
      , div [class "corner bottom"]
        [ span [class "number"] [text value]
        , span [] [text symbol]
        ]
      ]

showDeck : Deck -> Html.Html
showDeck deck =
  div [class "cards"] (List.map showCard deck)

model : Model
model =
  let deck = makeDeck
  in
    { prevCards = []
    , nextCards = drop 1 deck
    , card = head deck |> Maybe.withDefault (Hearts, "Ace")
    , control = NoOp
    }

update : Keys -> Model -> Model
update keys model =
  model
    |> control keys
    |> move

move : Model -> Model
move model =
  let
    noop = (model.card, model.prevCards, model.nextCards)
    (card, p, n) =
      case (model.control, head model.prevCards, head model.nextCards) of
        (NoOp, _, _) -> noop
        (Back, Nothing, _) -> noop
        (Next, _, Nothing) -> noop
        (Back, Just p, _) -> (p, drop 1 model.prevCards, model.card :: model.nextCards)
        (Next, _, Just n) -> (n, model.card :: model.prevCards, drop 1 model.nextCards)

  in { model |
        card <- card,
        prevCards <- p,
        nextCards <- n
     }

control : Keys -> Model -> Model
control keys model =
  { model |
      control <-
        if  | keys.x < 0 -> Back
            | keys.x > 0 -> Next
            | otherwise  -> NoOp
  }

input : Signal Keys
input = Keyboard.arrows

view : Model -> Element
view model =
  let card = div [class "cards"] [(showCard model.card)]
  in container 500 500 middle (toElement 400 400 card)

main =
  --Signal.map (centered << Text.fromString << toString) (Signal.foldp update model input)
  Signal.map view (Signal.foldp update model input)

