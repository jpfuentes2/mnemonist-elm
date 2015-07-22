import String
import Dict exposing (Dict)
import Array exposing (Array)
import Set
import Time exposing (every, second)
import Html exposing (..)
import Html.Attributes exposing (class)
import Keyboard
import Graphics.Element exposing (middle, container, down, flow, Element, show)
import Text
import Timer exposing (timer)
import PlayingCards exposing (..)

--type alias Model =
  --{ position: Int
  --, cards : Array Card
  --, numCards: Int
  --, card : Maybe Card
  --, control : Control
  --}

showRank : Card -> Html.Html
showRank card =
  let rank = showRank card.rank
      symbol = suitSymbol card.suit
      suitName = card.suit |> toString |> String.toLower
  in
    div [class ("card " ++ suitName)]
      [ div [class "corner top"]
        [ span [class "number"] [text rank]
        , span [] [text symbol]
        ]
      ]

showCards : Dict String (List Card) -> Html.Html
showCards groupedBySuits =
  div [class "cards recall"] (Dict.foldl showRank groupedBySuits)

--initialModel : Model
--initialModel =
  --let deck = makeDeck |> Array.fromList
  --in
    --{ position = 0
    --, cards = deck
    --, numCards = Array.length deck
    --, card = Array.get 0 deck
    --, control = NoOp
    --}

--update : Keys -> Model -> Model
--update keys model =
  --model |> control keys |> move

--succ : number -> number
--succ n = n + 1

--move : Model -> Model
--move model =
  --let
    --makeMove =
      --case model.control of
        --NoOp -> identity
        --Back -> if model.position - 1 < 0 then identity else pred
        --Next -> if model.position + 1 == model.numCards then identity else succ
    --position = makeMove model.position

  --in { model |
        --position <- position,
        --card <- Array.get position model.cards
     --}

--input : Signal Keys
--input = Keyboard.arrows

--view : Timer.Model -> Model -> Html
--view timer model =
  --let
      --card = case model.card of
        --Just c -> showCard c
        --Nothing -> div [class "card blank"] []
      --position = (toString <| model.position + 1) ++ " / " ++ (toString model.numCards)
  --in
    --div
      --[class "game container"]
      --[ div [class "timer"] [Timer.view timer]
      --, card
      --, div [class "position"] [Html.text position]
      ---- , div [] [model |> toString |> Html.text]
      --]-- |> down |> flow

---- manage the model of our application over time
--state : Signal Model
--state = Signal.foldp update initialModel input

main =
  let cards = makeDeck
      strSuits = List.map (\s -> ((toString s), [])) suits |> Dict.fromList
      append = (\c group -> case group of
        Just g -> Just <| c :: g
        Nothing -> Just [c])
      groups = List.foldl (\c -> Dict.update (toString c.suit) (append c)) strSuits cards
  --let x = List.foldl [] (\c -> c.suit == Hearts) makeDeck
  in show groups
  -- showCards makeDeck
  -- Signal.map2 view timer state

