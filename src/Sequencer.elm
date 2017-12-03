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
import Json.Encode as Encode
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
initButton = Enabled

type alias Model =
  { sequence : Array Button
  , playing : Int
  , currentTime : Time
  }

init : Model
init =
  { sequence = Array.repeat 8 initButton
  , playing = 0
  , currentTime = 0
  }



-- UPDATE

type Msg
  = Toggle Int
  | Tick Time
  | CurrentTime Time

toggleButton : Button -> Button
toggleButton button =
  case button of
    Enabled -> Disabled
    Disabled -> Enabled

updateButton : Int -> Model -> Button -> Model
updateButton index model button =
  { model | sequence = Array.set index button model.sequence }

update : (Encode.Value -> Cmd msg) -> Msg -> Model -> (Model, Cmd msg)
update output message model =
  case message of
    Toggle index ->
      ( Array.get index model.sequence
          |> Maybe.map toggleButton
          |> Maybe.map (updateButton index model)
          |> Maybe.withDefault model
      , Cmd.none
      )
    Tick _ ->
      ( model
      , AudioGraph.getCurrentTime |> output
      )
    CurrentTime time ->
      ( { model
        | playing = time / (Time.inSeconds interval)
            |> round
            |> flip (%) (Array.length model.sequence)
        , currentTime = time
        }
      , Cmd.none
      )



-- SUBSCRIPTIONS

subscriptions : ((Time -> Msg) -> Sub Msg) -> Model -> Sub Msg
subscriptions input model =
  -- will need to make this more accurate to account for drift
  Sub.batch
    [ Time.every interval Tick
    , input CurrentTime
    ]




-- GRAPH

buttonToGain : Time -> Button -> List AudioGraph.AudioParam
buttonToGain time button =
  case button of
    Enabled ->
      [ AudioGraph.linearRampToValueAtTime 0 (time + 0.01) -- required for mobile to sound correct
      , AudioGraph.linearRampToValueAtTime 1 (time + 0.011)
      , AudioGraph.linearRampToValueAtTime 0 (time + 0.25)
      ]
    Disabled -> [ AudioGraph.value 0 ]

graph : String -> AudioGraph.Destination -> Model -> AudioGraph
graph id output model =
  let
    rootId = id ++ "-0"
    lowPassId = id ++ "-lowpass"
    highPassId = id ++ "-highpass"
    gain = Array.get model.playing model.sequence
      |> Maybe.map (buttonToGain model.currentTime)
      |> Maybe.withDefault [ AudioGraph.value 0 ]
  in
    [ AudioGraph.audioNode rootId output
        <| AudioGraph.gain gain
    , AudioGraph.audioNode lowPassId (AudioGraph.connectTo rootId)
        <| AudioGraph.lowPassFilter 78
    , AudioGraph.audioNode highPassId (AudioGraph.connectTo lowPassId)
        <| AudioGraph.highPassFilter 136
    , AudioGraph.audioNode (id ++ "-noise1") (AudioGraph.connectTo highPassId)
        <| AudioGraph.pinkNoise
    , AudioGraph.audioNode (id ++ "-noise2") (AudioGraph.connectTo highPassId)
        <| AudioGraph.pinkNoise
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
