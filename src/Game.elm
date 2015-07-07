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
import Timer exposing (timer)
import PlayingCards exposing (..)

type Control = Next | Back | NoOp

type alias Model =
  { prevCards: Deck
  , nextCards : Deck
  , card : Card
  , control : Control
  }

type alias Keys = { x: Int, y: Int }

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
      --, span [class "suit top_center"] [text symbol]
      --, span [class "suit bottom_center"] [text symbol]
      , div [class "corner bottom"]
        [ span [class "number"] [text value]
        , span [] [text symbol]
        ]
      ]

showDeck : Deck -> Html.Html
showDeck deck =
  div [class "cards"] (List.map showCard deck)

initialModel : Model
initialModel =
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

view : Timer.Model -> Model -> Html
view time model =
  div
    [class "game"]
    [ div [class "timer"] [Timer.view time]
    , showCard model.card
    ]

-- manage the model of our application over time
state : Signal Model
state = Signal.foldp update initialModel input

main =
  Signal.map2 view timer state

