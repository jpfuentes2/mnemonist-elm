module Timer where

import Html exposing (Html)
import List exposing (head, drop)
import Maybe
import Signal
import String
import Time exposing (every, millisecond, second, minute, Time)

type alias Model = { minutes : Int , seconds : Int }

model : Model
model = { minutes = 0, seconds = 0 }

update : Time -> Model -> Model
update _ model =
  let secs = 1 + model.seconds
      mod = \m -> (secs % m) == 0
  in
      { model |
          seconds <- if secs == 60 then 0 else secs,
          minutes <- if mod 60 then 1 + model.minutes else model.minutes
      }

timer : Signal Model
timer = Signal.foldp update model (every second)

view : Model -> Html
view model =
  [model.minutes, model.seconds]
    |> List.map (\n -> if n < 10 then "0" ++ (toString n) else (toString n))
    |> String.join ":"
    |> Html.text

main =
  Signal.map view timer

