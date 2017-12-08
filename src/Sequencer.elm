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
import Element as El exposing (Element)
import Element.Attributes as A
import Element.Events as E
import Json.Encode as Encode
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
      [ AudioGraph.valueAtTime 0 (time + 0.02)
      , AudioGraph.linearRampToValueAtTime 1 (time + 0.025)
      , AudioGraph.linearRampToValueAtTime 0 (time + 0.2)
      ]
    Disabled -> [ AudioGraph.value 0 ]

drum : Float -> Time -> List AudioGraph.AudioParam
drum fundamental time =
  [ AudioGraph.valueAtTime (fundamental * 2) (time + 0.02)
  , AudioGraph.exponentialRampToValueAtTime fundamental (time + 0.15)
  ]

graph : String -> AudioGraph.Destination -> Model -> AudioGraph
graph id output model =
  let
    rootId = id ++ "-0"
    gain = Array.get model.playing model.sequence
      |> Maybe.map (buttonToGain model.currentTime)
      |> Maybe.withDefault [ AudioGraph.value 0 ]
  in
    [ AudioGraph.audioNode rootId output
        <| AudioGraph.gain gain
    , AudioGraph.audioNode (id ++ "-osc") (AudioGraph.connectTo rootId)
        <| AudioGraph.sineWave (drum 72 model.currentTime) 0
    ]



-- VIEW

button : Int -> Int -> Button -> Element () variation Msg
button playing index state =
  let
    isPlaying = index == playing
    color =
      case (state, isPlaying) of
        (Disabled, False) -> "#333333"
        (Enabled, False) -> "#666666"
        _ -> "#999999"
  in
    El.button ()
      [ A.width A.fill
      , A.height (12.5 |> A.percent)
      , A.inlineStyle
          [ ("backgroundColor", color)
          , ("border", "4px solid #333333")
          ]
      , E.onClick (Toggle index)
      ]
      El.empty

view : Model -> Element () variation Msg
view model =
  El.column ()
    [ A.height A.fill
    , A.width A.fill
    ]
    (Array.indexedMap (button model.playing) model.sequence |> Array.toList)
