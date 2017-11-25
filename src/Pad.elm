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
import Html.Events as E
import Time exposing (Time)
import TouchGroup


-- MODEL

type alias Note = (Float, Time)

type alias Model = TouchGroup.Model Note

note : Float -> Note
note frequency = (frequency, 0)

-- Eb3 G3 C4 Eb4 G4 C5 Eb5 G5
init : Model
init =
  Dict.fromList
    [ ("0-Eb3", note 155.56)
    , ("1-G3", note 196.00)
    , ("2-C4", note 261.63)
    , ("3-Eb4", note 311.13)
    , ("4-G4", note 392.00)
    , ("5-C5", note 523.25)
    , ("6-Eb5", note 622.26)
    , ("7-G5", note 783.99)
    ]
    |> TouchGroup.init



-- UPDATE

type alias Msg = TouchGroup.Msg Time

updateTime : Time -> Note -> Note
updateTime time (f, _) =
  (f, time)

update : Msg -> Model -> Model
update message model =
  TouchGroup.update updateTime message model



-- GRAPH

config : ADSR.Config
config =
  { attack = 0.1
  , decay = 0.2
  , sustain = 0.5
  , release = 0.5
  }

-- NOTE: Timings can take a delayed amount of time. If a delayed message is sent
-- to virual audio graph it ignores the udpate.

noteGraph : String -> AudioGraph.Destination -> (Bool, Note) -> AudioGraph
noteGraph id destination (isActive, (frequency, time)) =
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
reduce : String -> AudioGraph.Destination -> (String, (Bool, Note)) -> AudioGraph -> AudioGraph
reduce rootId destination (id, note) graph =
  List.append (noteGraph (rootId ++ "-" ++ id) destination note) graph

graph : String -> AudioGraph.Destination -> Model -> AudioGraph
graph id output model =
  List.foldl (reduce id output) [] (Dict.toList model)



-- VIEW

decodeTime : Decoder Time
decodeTime =
  Decode.at ["view", "document", "virtualAudioGraph", "currentTime"] Decode.float

view : Model -> Html Msg
view =
  TouchGroup.view decodeTime
