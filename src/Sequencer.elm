module Sequencer exposing
  ( Model
  , init
  , Msg
  , update
  , subscriptions
  , graph
  , view
  )

import AudioGraph exposing (AudioGraph)
import Color exposing (Color)
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Time exposing (Time)
import Array exposing (Array)


-- MODEL

interval : Time
interval = 500 * Time.millisecond

type Button
  = Enabled
  | Disabled

initButton : Button
initButton = Disabled

type alias Model =
  { sequence : Array Button
  , playing : Int
  }

init : Model
init =
  { sequence = Array.repeat 8 initButton
  , playing = 0
  }



-- UPDATE

type Msg
  = Toggle Int
  | Tick Time

toggleButton : Button -> Button
toggleButton button =
  case button of
    Enabled -> Disabled
    Disabled -> Enabled

updateButton : Int -> Model -> Button -> Model
updateButton index model button =
  { model | sequence = Array.set index button model.sequence }

update : Msg -> Model -> Model
update message model =
  case message of
    Toggle index ->
      Array.get index model.sequence
        |> Maybe.map toggleButton
        |> Maybe.map (updateButton index model)
        |> Maybe.withDefault model
    Tick time ->
      { model
      | playing = time / interval
          |> round
          |> flip (%) (Array.length model.sequence)
      }



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  -- will need to make this more accurate to account for drift
  Time.every interval Tick



-- GRAPH

frequencyRatio : Float -> Float
frequencyRatio value =
  (2 ^ value) ^ (1 / 12)

buttonToGain : Button -> Float
buttonToGain button =
  case button of
    Enabled -> 0.5
    Disabled -> 0

graph : String -> AudioGraph.Destination -> Model -> AudioGraph
graph id output model =
  let
    rootId = id ++ "-0"
    frequency = (Array.length model.sequence |> toFloat) / (model.playing + 1 |> toFloat)
    gain = Array.get model.playing model.sequence
      |> Maybe.map buttonToGain
      |> Maybe.withDefault 0
  in
    [ AudioGraph.gainNode rootId output
        [ AudioGraph.gain gain ]
    , AudioGraph.oscillator (id ++ "-1") (AudioGraph.connectTo rootId)
        [ AudioGraph.sineWave
        , frequencyRatio frequency |> (*) 110 |> AudioGraph.frequency
        ]
    , AudioGraph.oscillator (id ++ "-2") (AudioGraph.connectTo rootId)
        [ AudioGraph.sineWave
        , frequency + 7 |> frequencyRatio |> (*) 110 |> AudioGraph.frequency
        , AudioGraph.detune 4
        ]
    ]



-- VIEW

button : Int -> Int -> Button -> Html Msg
button playing index state =
  let
    isPlaying = index == playing
    color =
      case (state, isPlaying) of
        (Disabled, False) -> "#333333"
        (Enabled, False) -> "#666666"
        _ -> "#999999"
  in
    H.button
      [ A.style
          [ ("backgroundColor", color)
          , ("width", "50px")
          , ("height", "50px")
          ]
      , E.onClick (Toggle index)
      ]
      []

controls : Int -> Array Button -> Html Msg
controls playing buttons =
  H.div
    [ A.style []
    ]
    (Array.indexedMap (button playing) buttons |> Array.toList)



view : Model -> Html Msg
view model =
  H.div
    [ A.style []
    ]
    [ controls model.playing model.sequence
    ]
