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
import Element as El exposing (Element)
import Element.Attributes as A
import Element.Events as E
import Json.Decode as Decode exposing (Decoder)
import Time exposing (Time)
import TouchGroup



-- MODEL

type State
  = Active
  | Inactive

type alias Note = (State, Float, Time)

type alias Model = (TouchGroup.Model, Dict String Note)

note : Float -> Note
note frequency = (Inactive, frequency, 0)

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
  (Active, frequency, time)

onStart : String -> Time -> Dict String Note -> Dict String Note
onStart key time model =
  Dict.update key (Maybe.map (startNote time)) model

stopNote : Time -> Note -> Note
stopNote time (isActive, frequency, oldTime) =
  (Inactive, frequency, time)

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
noteGraph id destination (state, frequency, time) =
  let
    gainId = id ++ "-0"
    gainProperties =
      case state of
        Active -> ADSR.on config time
        Inactive -> ADSR.off config time
  in
    [ AudioGraph.audioNode gainId destination (AudioGraph.gain gainProperties)
    , AudioGraph.audioNode (id ++ "-1") (AudioGraph.connectTo gainId)
        <| AudioGraph.squareWave [AudioGraph.value frequency] 0
    , AudioGraph.audioNode (id ++ "-2") (AudioGraph.connectTo gainId)
        <| AudioGraph.squareWave [AudioGraph.value frequency] 10
    , AudioGraph.audioNode (id ++ "-2") (AudioGraph.connectTo gainId)
        <| AudioGraph.squareWave [AudioGraph.value frequency] -10
    ]

-- change name
notesGraph : String -> AudioGraph.Destination -> (String, Note) -> AudioGraph -> AudioGraph
notesGraph rootId destination (id, note) graph =
  List.append (noteGraph (rootId ++ "-" ++ id) destination note) graph

graph : String -> AudioGraph.Destination -> Model -> AudioGraph
graph id output (_, notes) =
  let
    gainId = id ++ "-0"
    things = List.foldl (notesGraph id (AudioGraph.connectTo gainId)) [] (Dict.toList notes)
  in
    AudioGraph.audioNode gainId output (AudioGraph.gain [AudioGraph.value 0.2])
      :: things



-- VIEW

itemView : (String, Note) -> Element () variation Msg
itemView (key, (state, _, _)) =
  let
    color =
      case state of
        Active -> "#999999"
        Inactive -> "#666666"
  in
    El.button ()
      [ TouchGroup.key key |> A.toAttr
      , A.width A.fill
      , A.height (12.5 |> A.percent)
      , A.inlineStyle
          [ ("backgroundColor", color)
          , ("border", "4px solid #333333")
          ]
      ]
      El.empty

view : Model -> Element () variation Msg
view (_, notes) =
  El.column ()
    ( [ A.height A.fill
      , A.width A.fill
      ]
      |> List.append (TouchGroup.onTouch AudioGraph.decodeTime |> List.map A.toAttr)
    )
    (Dict.toList notes |> List.map itemView)
