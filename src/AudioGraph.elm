module AudioGraph exposing
  ( AudioGraph
  , audioGraph
  , none
  , AudioNode
  , audioNode
  , Destination
  , connectTo, output
  , AudioParam
  , sineWave, squareWave, sawtoothWave, triangleWave
  , gain
  , lowPassFilter, highPassFilter
  , pinkNoise
  , value, valueAtTime, linearRampToValueAtTime, exponentialRampToValueAtTime
  , updateGraph
  , getCurrentTime
  , decodeTime
  )

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Time exposing (Time)


type alias AudioGraph = List AudioNode

type alias AudioNode =
  { id : String
  , destination : Destination
  , node : Node
  }

type Node
  = OscillatorNode WaveType Frequency Detune
  | GainNode Gain
  | BiquadFilterNode FilterType Frequency
  | PinkNoiseNode

type WaveType
  = Sine
  | Square
  | Sawtooth
  | Triangle

type FilterType
  = LowPass
  | HighPass

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

none : AudioGraph
none = []

audioNode : String -> Destination -> Node -> AudioNode
audioNode = AudioNode

sineWave : List AudioParam -> Float -> Node
sineWave frequency detune =
  OscillatorNode
    Sine
    (Frequency frequency)
    (Detune [Scalar detune])

squareWave : List AudioParam -> Float -> Node
squareWave frequency detune =
  OscillatorNode
    Square
    (Frequency frequency)
    (Detune [Scalar detune])

sawtoothWave : List AudioParam -> Float -> Node
sawtoothWave frequency detune =
  OscillatorNode
    Sawtooth
    (Frequency frequency)
    (Detune [Scalar detune])

triangleWave : List AudioParam -> Float -> Node
triangleWave frequency detune =
  OscillatorNode
    Triangle
    (Frequency frequency)
    (Detune [Scalar detune])

gain : List AudioParam -> Node
gain params =
  GainNode (Gain params)

lowPassFilter : Float -> Node
lowPassFilter frequency =
  BiquadFilterNode
    LowPass
    (Frequency [Scalar frequency])

highPassFilter : Float -> Node
highPassFilter frequency =
  BiquadFilterNode
    LowPass
    (Frequency [Scalar frequency])

pinkNoise : Node
pinkNoise = PinkNoiseNode

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
    Triangle -> Encode.string "triangle"

encodeFilterType : FilterType -> Encode.Value
encodeFilterType filterType =
  case filterType of
    LowPass -> Encode.string "lowpass"
    HighPass -> Encode.string "highpass"

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
    BiquadFilterNode filterType (Frequency frequency) ->
      ( "biquadFilter"
      , [ ( "type", encodeFilterType filterType )
        , ( "frequency", encodeAudioParams frequency )
        ]
        |> Encode.object
      )
    PinkNoiseNode -> ("pinkNoise", Encode.null)

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

updateGraph : AudioGraph -> Encode.Value
updateGraph graph =
  [ ("type", Encode.string "update")
  , ("data", List.map encodeAudioNode graph |> Encode.object)
  ]
    |> Encode.object

getCurrentTime : Encode.Value
getCurrentTime =
  [ ("type", Encode.string "getCurrentTime") ] |> Encode.object

-- Decode Time

decodeTime : Decoder Time
decodeTime =
  Decode.at ["view", "document", "audioGraph", "currentTime"] Decode.float
