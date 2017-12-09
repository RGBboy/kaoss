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

steps : Int
steps = 16

type Button
  = Enabled
  | Disabled

initButton : Int -> Button
initButton index =
  if (index % 4 == 0) then
    Enabled
  else
    Disabled

type alias Sequence = Array Button

initSequence : Sequence
initSequence =
  Array.initialize steps initButton

type alias Model =
  { interval : Time
  , tracks : Array Sequence
  , playing : Int
  , currentTime : Time
  }

init : Int -> Model
init tracks =
  { interval = Time.minute / (100 * 4)
  , tracks = Array.repeat tracks initSequence
  , playing = 0
  , currentTime = 0
  }



-- UPDATE

type Msg
  = Toggle Int Int
  | Tick Time
  | CurrentTime Time

updateArray : (Maybe a -> Maybe a) -> Int -> Array a -> Array a
updateArray update index array =
  Array.get index array
    |> update
    |> Maybe.map (\a -> Array.set index a array)
    |> Maybe.withDefault array

toggleButton : Button -> Button
toggleButton button =
  case button of
    Enabled -> Disabled
    Disabled -> Enabled

toggleButtonInSequence : Int -> Sequence -> Sequence
toggleButtonInSequence index sequence =
  updateArray (Maybe.map toggleButton) index sequence

toggleButtonInTrack : Int -> Int -> Array Sequence -> Array Sequence
toggleButtonInTrack track index tracks =
  updateArray (Maybe.map <| toggleButtonInSequence index) track tracks

update : (Encode.Value -> Cmd msg) -> Msg -> Model -> (Model, Cmd msg)
update output message model =
  case message of
    Toggle track index ->
      ( { model
        | tracks = toggleButtonInTrack track index model.tracks
        }
      , Cmd.none
      )
    Tick _ ->
      ( model
      , AudioGraph.getCurrentTime |> output
      )
    CurrentTime time ->
      ( { model
        | playing = time / (Time.inSeconds model.interval)
            |> round
            |> flip (%) steps
        , currentTime = time
        }
      , Cmd.none
      )



-- SUBSCRIPTIONS

subscriptions : ((Time -> Msg) -> Sub Msg) -> Model -> Sub Msg
subscriptions input model =
  -- will need to make this more accurate to account for drift
  Sub.batch
    [ Time.every model.interval Tick
    , input CurrentTime
    ]



-- GRAPH

mapButton : a -> a -> Button -> a
mapButton on off button =
  case button of
    Enabled -> on
    Disabled -> off

graphSequence : (Int -> AudioGraph) -> Int -> Int -> Sequence -> AudioGraph
graphSequence createGraph playing track sequence =
  Array.get playing sequence
    |> Maybe.map (mapButton (createGraph track) AudioGraph.none)
    |> Maybe.withDefault AudioGraph.none

graph : (Time -> Int -> AudioGraph) -> Model -> AudioGraph
graph createGraph model =
  model.tracks
    |> Array.indexedMap (graphSequence (createGraph model.currentTime) model.playing)
    |> Array.foldl List.append AudioGraph.none



-- VIEW

buttonView : Int -> Int -> Int -> Button -> Element () variation Msg
buttonView playing track index state =
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
      , A.height (A.fillPortion 1)
      , A.inlineStyle
          [ ("backgroundColor", color)
          , ("border", "4px solid #333333")
          ]
      , E.onClick (Toggle track index)
      ]
      El.empty

trackView : Int -> Int -> Sequence -> Element () variation Msg
trackView playing track sequence =
  El.column ()
    [ A.height A.fill
    , A.width A.fill
    ]
    (Array.indexedMap (buttonView playing track) sequence |> Array.toList)

view : Model -> Element () variation Msg
view model =
  El.row ()
    [ A.height A.fill
    , A.width A.fill
    ]
    (Array.indexedMap (trackView model.playing) model.tracks |> Array.toList)
