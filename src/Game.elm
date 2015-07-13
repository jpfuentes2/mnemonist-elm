import String
import Array exposing (Array)
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
  { position: Int
  , cards : Array Card
  , numCards: Int
  , card : Maybe Card
  , control : Control
  }

type alias Keys = { x: Int, y: Int }

showCard : Card -> Html.Html
showCard (suit, r) =
  let rank = showRank r
      symbol = suitSymbol suit
      suitName = suit |> toString |> String.toLower
  in
    div [class ("card " ++ suitName)]
      [ div [class "corner top"]
        [ span [class "number"] [text rank]
        , span [] [text symbol]
        ]
      --, span [class "suit top_center"] [text symbol]
      --, span [class "suit bottom_center"] [text symbol]
      , div [class "corner bottom"]
        [ span [class "number"] [text rank]
        , span [] [text symbol]
        ]
      ]

showCards : Cards -> Html.Html
showCards cards =
  div [class "cards"] (List.map showCard cards)

initialModel : Model
initialModel =
  let deck = makeDeck |> Array.fromList
  in
    { position = 0
    , cards = deck
    , numCards = Array.length deck
    , card = Array.get 0 deck
    , control = NoOp
    }

update : Keys -> Model -> Model
update keys model =
  model |> control keys |> move

succ : number -> number
succ n = n + 1

pred : number -> number
pred n = n - 1

move : Model -> Model
move model =
  let
    makeMove =
      case model.control of
        NoOp -> identity
        Back -> if model.position - 1 < 0 then identity else pred
        Next -> if model.position + 1 == model.numCards then identity else succ
    position = makeMove model.position

  in { model |
        position <- position,
        card <- Array.get position model.cards
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
view timer model =
  let
      card = case model.card of
        Just c -> showCard c
        Nothing -> div [class "card blank"] []
      position = (toString <| model.position + 1) ++ " / " ++ (toString model.numCards)
  in
    div
      [class "game"]
      [ div [class "timer"] [Timer.view timer]
      , card
      , div [class "position"] [Html.text position]
      , div [] [model |> toString |> Html.text]
      ]

-- manage the model of our application over time
state : Signal Model
state = Signal.foldp update initialModel input

main =
  Signal.map2 view timer state

