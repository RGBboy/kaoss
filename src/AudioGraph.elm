module AudioGraph exposing
  ( AudioGraph
  , audioGraph
  , audioNode
  , Destination
  , connectTo, output
  , AudioParam
  , sineWave, squareWave, sawtoothWave
  , gain
  , value, valueAtTime, linearRampToValueAtTime, exponentialRampToValueAtTime
  , encode
  )

import Dict exposing (Dict)
import Json.Encode as Encode



type alias AudioGraph = List AudioNode

type alias AudioNode =
  { id : String
  , destination : Destination
  , node : Node
  }

type Node
  = OscillatorNode WaveType Frequency Detune
  | GainNode Gain

type WaveType
  = Sine
  | Square
  | Sawtooth

type Frequency =
  Frequency (List AudioParam)

type Detune =
  Detune (List AudioParam)

type Gain =
  Gain (List AudioParam)

type Destination
  = Output
  | Connect String

type AudioParam
  = Scalar Float
  | ValueAtTime Float Float
  | LinearRampToValueAtTime Float Float
  | ExponentialRampToValueAtTime Float Float

audioGraph : List AudioNode -> AudioGraph
audioGraph = identity

audioNode : String -> Destination -> Node -> AudioNode
audioNode = AudioNode

sineWave : Float -> Float -> Node
sineWave frequency detune =
  OscillatorNode
    Sine
    (Frequency [Scalar frequency])
    (Detune [Scalar detune])

squareWave : Float -> Float -> Node
squareWave frequency detune =
  OscillatorNode
    Square
    (Frequency [Scalar frequency])
    (Detune [Scalar detune])

sawtoothWave : Float -> Float -> Node
sawtoothWave frequency detune =
  OscillatorNode
    Sawtooth
    (Frequency [Scalar frequency])
    (Detune [Scalar detune])

gain : List AudioParam -> Node
gain params =
  GainNode (Gain params)

connectTo : String -> Destination
connectTo = Connect

output : Destination
output = Output

value : Float -> AudioParam
value v = Scalar v

valueAtTime : Float -> Float -> AudioParam
valueAtTime v time = ValueAtTime v time

linearRampToValueAtTime : Float -> Float -> AudioParam
linearRampToValueAtTime v time = LinearRampToValueAtTime v time

exponentialRampToValueAtTime : Float -> Float -> AudioParam
exponentialRampToValueAtTime v time = ExponentialRampToValueAtTime v time



-- ENCODER

encodeDestination : Destination -> Encode.Value
encodeDestination destination =
  case destination of
    Output -> Encode.string "output"
    Connect id -> Encode.string id

encodeAudioParam : AudioParam -> Encode.Value
encodeAudioParam value =
  case value of
    Scalar v -> Encode.float v
    ValueAtTime v t ->
      Encode.list
        [ Encode.string "setValueAtTime"
        , Encode.float v
        , Encode.float t
        ]
    LinearRampToValueAtTime v t ->
      Encode.list
        [ Encode.string "linearRampToValueAtTime"
        , Encode.float v
        , Encode.float t
        ]
    ExponentialRampToValueAtTime v t ->
      Encode.list
        [ Encode.string "exponentialRampToValueAtTime"
        , Encode.float v
        , Encode.float t
        ]

-- Temporary -- Will need to be updated to work correctly with properties that should just have a single value and properties that can have multiple
encodeAudioParams : List AudioParam -> Encode.Value
encodeAudioParams params =
  let
    list = List.map encodeAudioParam params
  in
    case list of
      a::b::_ -> Encode.list list
      a::_ -> a
      [] -> Encode.null

encodeWaveType : WaveType -> Encode.Value
encodeWaveType waveType =
  case waveType of
    Sine -> Encode.string "sine"
    Square -> Encode.string "square"
    Sawtooth -> Encode.string "sawtooth"

encodeNode : Node -> (String, Encode.Value)
encodeNode node =
  case node of
    OscillatorNode waveType (Frequency frequency) (Detune detune) ->
      ( "oscillator"
      , [ ( "type", encodeWaveType waveType )
        , ( "frequency", encodeAudioParams frequency )
        , ( "detune", encodeAudioParams detune )
        ]
        |> Encode.object
      )
    GainNode (Gain gain) ->
      ( "gain"
      , [ ("gain", encodeAudioParams gain)
        ]
        |> Encode.object
      )

encodeAudioNode : AudioNode -> (String, Encode.Value)
encodeAudioNode audioNode =
  let
    (kind, audioParams) = encodeNode audioNode.node
  in

    ( audioNode.id
    , Encode.list
        [ Encode.string kind
        , encodeDestination audioNode.destination
        , audioParams
        ]
    )

encode : AudioGraph -> Encode.Value
encode graph =
  List.map encodeAudioNode graph |> Encode.object
