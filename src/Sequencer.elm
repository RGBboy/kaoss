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
    [ AudioGraph.audioNode rootId output
        <| AudioGraph.gain [AudioGraph.value gain]
    , AudioGraph.audioNode (id ++ "-1") (AudioGraph.connectTo rootId)
        <| AudioGraph.sineWave (frequencyRatio frequency |> (*) 110) 0
    , AudioGraph.audioNode (id ++ "-2") (AudioGraph.connectTo rootId)
        <| AudioGraph.sineWave (frequency + 7 |> frequencyRatio |> (*) 110) 4
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
          , ("box-sizing", "border-box")
          , ("border", "4px solid #333333")
          , ("width", "100%")
          , ("height", "12.5%")
          ]
      , E.onClick (Toggle index)
      ]
      []

controls : Int -> Array Button -> Html Msg
controls playing buttons =
  H.div
    [ A.style
        [ ("width", "100%")
        , ("height", "100%")
        ]
    ]
    (Array.indexedMap (button playing) buttons |> Array.toList)



view : Model -> Html Msg
view model =
  H.div
    [ A.style
        [ ("width", "20%")
        , ("height", "100%")
        , ("float", "left")
        ]
    ]
    [ controls model.playing model.sequence
    ]
