module Pad exposing
  ( Model
  , init
  , Msg
  , update
  , graph
  , view
  )

import ADSR
import AudioGraph exposing (AudioGraph)
import Color exposing (Color)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Html as H exposing (Html)
import Html.Attributes as A
import Time exposing (Time)
import TouchGroup



-- MODEL

type alias Note = (Bool, Float, Time)

type alias Model = (TouchGroup.Model, Dict String Note)

note : Float -> Note
note frequency = (False, frequency, 0)

notes : Dict String Note
notes = Dict.fromList
  [ ("0-Eb3", note 155.56)
  , ("1-G3", note 196.00)
  , ("2-C4", note 261.63)
  , ("3-Eb4", note 311.13)
  , ("4-G4", note 392.00)
  , ("5-C5", note 523.25)
  , ("6-Eb5", note 622.26)
  , ("7-G5", note 783.99)
  ]

init : Model
init = (TouchGroup.init, notes)



-- UPDATE

type alias Msg = TouchGroup.Msg Time

startNote : Time -> Note -> Note
startNote time (isActive, frequency, oldTime) =
  (True, frequency, time)

onStart : String -> Time -> Dict String Note -> Dict String Note
onStart key time model =
  Dict.update key (Maybe.map (startNote time)) model

stopNote : Time -> Note -> Note
stopNote time (isActive, frequency, oldTime) =
  (False, frequency, time)

onStop : String -> Time -> Dict String Note -> Dict String Note
onStop key time model =
  Dict.update key (Maybe.map (stopNote time)) model

update : Msg -> Model -> Model
update message model =
  TouchGroup.update onStart onStop message model



-- GRAPH

config : ADSR.Config
config =
  { attack = 0.1
  , decay = 0.2
  , sustain = 0.5
  , release = 0.5
  }

noteGraph : String -> AudioGraph.Destination -> Note -> AudioGraph
noteGraph id destination (isActive, frequency, time) =
  let
    gainId = id ++ "-0"
    gainProperties =
      case isActive of
        True -> ADSR.on config time
        False -> ADSR.off config time
  in
    [ AudioGraph.gainNode gainId destination gainProperties
    , AudioGraph.oscillator (id ++ "-1") (AudioGraph.connectTo gainId)
        [ AudioGraph.squareWave
        , AudioGraph.frequency frequency
        ]
    , AudioGraph.oscillator (id ++ "-2") (AudioGraph.connectTo gainId)
        [ AudioGraph.squareWave
        , AudioGraph.detune 10
        , AudioGraph.frequency frequency
        ]
    , AudioGraph.oscillator (id ++ "-2") (AudioGraph.connectTo gainId)
        [ AudioGraph.squareWave
        , AudioGraph.detune -10
        , AudioGraph.frequency frequency
        ]
    ]

-- change name
notesGraph : String -> AudioGraph.Destination -> (String, Note) -> AudioGraph -> AudioGraph
notesGraph rootId destination (id, note) graph =
  List.append (noteGraph (rootId ++ "-" ++ id) destination note) graph

graph : String -> AudioGraph.Destination -> Model -> AudioGraph
graph id output (_, notes) =
  List.foldl (notesGraph id output) [] (Dict.toList notes)



-- VIEW

decodeTime : Decoder Time
decodeTime =
  Decode.at ["view", "document", "virtualAudioGraph", "currentTime"] Decode.float

itemView : (String, Note) -> Html Msg
itemView (key, (isActive, _, _)) =
  let
    color =
      if isActive == True then
        "#999999"
      else
        "#666666"
  in
    TouchGroup.item key
      [ A.style
          [ ("backgroundColor", color)
          , ("box-sizing", "border-box")
          , ("border", "4px solid #333333")
          , ("width", "100%")
          , ("height", "12.5%")
          , ("float", "left")
          ]
      ]
      []

view : Model -> Html Msg
view (_, notes) =
  TouchGroup.group decodeTime
    [ A.style
        [ ("backgroundColor", "#333333")
        , ("width", "100%")
        , ("height", "100%")
        ]
    ]
    (Dict.toList notes |> List.map itemView)
