port module Main exposing (..)

import Color exposing (Color)
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as Events
import Json.Encode as Encode
import Mouse
import Time
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

port output : Output -> Cmd msg

type alias Output = (Bool, Float, Float)

toOutput : Model -> Output
toOutput { active, position, dimensions } =
  let
    (x, y) = normalize position dimensions
    isActive = (active && x < 1 && x > 0 && y < 1 && y > 0)
  in
    (isActive, x, y)



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

type alias Model =
  { active : Bool
  , position : (Int, Int)
  , dimensions : (Int, Int)
  , graph : AudioGraph
  }

init : (Model, Cmd msg)
init =
  ( { active = False
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

graph : Float -> Float -> AudioGraph
graph g f =
  [ gain "0" Output [ Gain (g * g) ]
  , oscillator "1" (Connect "0")
      [ WaveType Square
      , frequencyRatio f |> (*) 110 |> Frequency
      ]
  , oscillator "2" (Connect "0")
      [ WaveType Sawtooth
      , f + 7 |> frequencyRatio |> (*) 110 |> Frequency
      , Detune 4
      ]
  ]

frequencyRatio : Float -> Float
frequencyRatio value =
  (2 ^ value) ^ (1 / 12)


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



-- UPDATE

type Msg
  = Active Bool
  | Position Mouse.Position
  | Resize Window.Size

normalize : (Int, Int) -> (Int, Int) -> (Float, Float)
normalize (xInt, yInt) (wInt, hInt) =
  let
    (w, h) = (toFloat wInt, toFloat hInt)
    (ox, oy) = ((w - 320) / 2, (h - 320) / 2)
    (x, y) = (toFloat xInt, toFloat yInt)
  in
    ((x - ox) / 320, (y - oy) / 320)



update : Msg -> Model -> (Model, Cmd msg)
update message ({ active, position, dimensions } as model) =
  case message of
    Active v ->
      let
        newModel = { model | active = v }
      in
        ( newModel
        , output (toOutput newModel)
        )
    Position { x, y } ->
      let
        newModel = { model | position = (x, y) }
      in
        ( newModel
        , output (toOutput newModel)
        )
    Resize { width, height } ->
      let
        newModel = { model | dimensions = (width, height) }
      in
        ( newModel
        , output (toOutput newModel)
        )



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  let
    moves =
      if model.active then
        Mouse.moves Position
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
view { dimensions } =
  let
    (width, height) = dimensions
    (w, h) = (toString width, toString height)
    (x, y) = (toString (toFloat (width - 320) / 2), toString (toFloat (height - 320) / 2))
  in
    H.div
      [ A.style
          [ ("backgroundColor", "#333333")
          , ("width", w ++ "px")
          , ("height", h ++ "px")
          ]
      ]
      [ H.div
          [ A.style
              [ ("backgroundColor", "#000000")
              , ("position", "absolute")
              , ("left", x ++ "px")
              , ("top", y ++ "px")
              , ("width", "320px")
              , ("height", "320px")
              ]
          ]
          []
      ]
