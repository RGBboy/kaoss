port module Main exposing (..)

import Color exposing (Color)
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Json.Encode as Encode
import Mouse
import SingleTouch
import Time
import Touch
import Window



main : Program Never Model Msg
main =
  H.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- PORTS

port output : Encode.Value -> Cmd msg

-- MODEL

type NodeType
  = OscillatorNode
  | GainNode

type Destination
  = Output
  | Connect String

type AudioProperty
  = WaveType Wave
  | Frequency Float
  | Gain Float
  | Detune Int

type Wave
  = Sine
  | Square
  | Sawtooth

type alias AudioNode =
  { nodeType : NodeType
  , id : String
  , destination : Destination
  , properties : List AudioProperty
  }

type alias AudioGraph = List AudioNode

type State
  = Idle
  | Playing

type alias Model =
  { state : State
  , isActive : Bool
  , position : (Float, Float)
  , dimensions : (Float, Float)
  , graph : AudioGraph
  }

init : (Model, Cmd msg)
init =
  ( { state = Idle
    , isActive = False
    , position = (0, 0)
    , dimensions = (320, 320)
    , graph = []
    }
  , Cmd.none
  )

audioNode : NodeType -> String -> Destination -> List AudioProperty -> AudioNode
audioNode = AudioNode

oscillator : String -> Destination -> List AudioProperty -> AudioNode
oscillator = AudioNode OscillatorNode

gain : String -> Destination -> List AudioProperty -> AudioNode
gain = AudioNode GainNode

encodeNodeType : NodeType -> Encode.Value
encodeNodeType nodeType =
  case nodeType of
    OscillatorNode -> Encode.string "oscillator"
    GainNode -> Encode.string "gain"

encodeDestination : Destination -> Encode.Value
encodeDestination destination =
  case destination of
    Output -> Encode.string "output"
    Connect id -> Encode.string id

encodeAudioProperty : AudioProperty -> (String, Encode.Value)
encodeAudioProperty property =
  case property of
    WaveType value -> ("type", encodeWave value)
    Frequency value -> ("frequency", Encode.float value)
    Gain value -> ("gain", Encode.float value)
    Detune value -> ("detune", Encode.int value)

encodeWave : Wave -> Encode.Value
encodeWave wave =
  case wave of
    Sine -> Encode.string "sine"
    Square -> Encode.string "square"
    Sawtooth -> Encode.string "sawtooth"

encodeNode : AudioNode -> (String, Encode.Value)
encodeNode node =
  ( node.id
  , Encode.list
      [ encodeNodeType node.nodeType
      , encodeDestination node.destination
      , List.map encodeAudioProperty node.properties |> Encode.object
      ]
  )

encodeGraph : AudioGraph -> Encode.Value
encodeGraph graph =
  List.map encodeNode graph |> Encode.object

outputType : String -> Encode.Value -> Encode.Value
outputType kind data =
  [ ("type", Encode.string kind)
  , ("data", data)
  ]
    |> Encode.object



-- UPDATE

type Msg
  = Start
  | Active Bool
  | Position (Float, Float)
  | Resize Window.Size

normalize : (Float, Float) -> (Float, Float) -> (Float, Float)
normalize (aX, aY) (bX, bY) =
  (aX / bX, aY / bY)

scale : number -> (number, number) -> (number, number)
scale factor (x, y) =
  (x * factor, y * factor)

clamp : (number, number) -> (number, number) -> (number, number) -> (number, number)
clamp (minX, minY) (maxX, maxY) (x, y) =
  (Basics.clamp minX maxX x, Basics.clamp minY maxY y)

boolToFloat : Bool -> number
boolToFloat value =
  case value of
    True -> 1
    False -> 0

frequencyRatio : Float -> Float
frequencyRatio value =
  (2 ^ value) ^ (1 / 12)

graph : Float -> Float -> AudioGraph
graph g f =
  [ gain "0" Output [ Gain (g * g * 0.5) ]
  , oscillator "1" (Connect "0")
      [ WaveType Square
      , frequencyRatio f |> (*) 110 |> Frequency
      ]
  , oscillator "2" (Connect "0")
      [ WaveType Square
      , f + 7 |> frequencyRatio |> (*) 110 |> Frequency
      , Detune 4
      ]
  ]

createGraph : Bool -> (Float, Float) -> (Float, Float) -> AudioGraph
createGraph isActive dimensions position =
  normalize position dimensions
    |> clamp (0, 0) (1, 1)
    |> scale (boolToFloat isActive)
    |> (uncurry graph)

update : Msg -> Model -> (Model, Cmd msg)
update message ({ isActive, position, dimensions } as model) =
  case message of
    Start ->
      ( { model | state = Playing }
      , outputType "init" Encode.null |> output
      )
    Active isActive ->
      let
        newModel =
          { model
          | isActive = isActive
          , graph = createGraph isActive model.dimensions model.position
          }
      in
        ( newModel
        , encodeGraph newModel.graph |> outputType "update" |> output
        )
    Position position ->
      let
        newModel =
          { model
          | position = position
          , graph = createGraph model.isActive model.dimensions position
          }
      in
        ( newModel
        , encodeGraph newModel.graph |> outputType "update" |> output
        )
    Resize { width, height } ->
      let
        dimensions = (toFloat width, toFloat height)
        newModel =
          { model
          | dimensions = dimensions
          , graph = createGraph model.isActive dimensions model.position
          }
      in
        ( newModel
        , encodeGraph newModel.graph |> outputType "update" |> output
        )



-- SUBSCRIPTIONS

tupleFromXY : { x : number, y : number } -> (number, number)
tupleFromXY { x, y } =
  (x, y)

intToFloat : (Int, Int) -> (Float, Float)
intToFloat (x, y) =
  (toFloat x, toFloat y)

subscriptions : Model -> Sub Msg
subscriptions model =
  case model.state of
    Idle ->
      Window.resizes Resize
    Playing ->
      let
        moves =
          if model.isActive then
            Mouse.moves (tupleFromXY >> intToFloat >> Position)
          else
            Sub.none
      in
        Sub.batch
          [ Window.resizes Resize
          , Mouse.downs (always (Active True))
          , Mouse.ups (always (Active False))
          , moves
          ]



-- VIEW

view : Model -> Html Msg
view { state, isActive, dimensions } =
  let
    (width, height) = dimensions
    (w, h) = (toString width, toString height)
    (x, y) = (toString ((width - 320) / 2), toString ((height - 320) / 2))
    color =
      case isActive of
        True -> "#333333"
        False -> "#000000"
  in
    case state of
      Idle ->
        H.button
          [ E.onClick Start
          ]
          [ H.text "Start" ]
      _ ->
        H.div
          [ A.style
              [ ("backgroundColor", "#333333")
              , ("width", w ++ "px")
              , ("height", h ++ "px")
              ]
          ]
          [ H.div
              [ A.style
                  [ ("backgroundColor", color)
                  , ("position", "absolute")
                  , ("left", x ++ "px")
                  , ("top", y ++ "px")
                  , ("width", "320px")
                  , ("height", "320px")
                  ]
              , SingleTouch.onStart (always (Active True))
              , SingleTouch.onEnd (always (Active False))
              , SingleTouch.onCancel (always (Active False))
              , SingleTouch.onMove (Touch.clientPos >> Position)
              ]
              []
          ]
