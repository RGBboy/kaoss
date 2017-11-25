module AudioGraph exposing
  ( AudioGraph
  , audioGraph
  , audioNode
  , oscillator, gainNode
  , Destination
  , connectTo, output
  , AudioProperty
  , sineWave, squareWave, sawtoothWave
  , frequency, gain, gainLinearRampToValueAtTime, gainValueAtTime, gainExponentialRampToValueAtTime, detune
  , encode
  )

import Dict exposing (Dict)
import Json.Encode as Encode



type alias AudioGraph = List AudioNode

type alias AudioNode =
  { nodeType : NodeType
  , id : String
  , destination : Destination
  , properties : List AudioProperty
  }

type NodeType
  = OscillatorNode
  | GainNode

type Destination
  = Output
  | Connect String

type Value
  = Scalar Float
  | ValueAtTime Float Float
  | LinearRampToValueAtTime Float Float
  | ExponentialRampToValueAtTime Float Float

type AudioProperty
  = WaveType Wave
  | Frequency Float
  | Gain Value
  | Detune Int

type Wave
  = Sine
  | Square
  | Sawtooth

audioGraph : List AudioNode -> AudioGraph
audioGraph = identity

audioNode : NodeType -> String -> Destination -> List AudioProperty -> AudioNode
audioNode = AudioNode

oscillator : String -> Destination -> List AudioProperty -> AudioNode
oscillator = AudioNode OscillatorNode

gainNode : String -> Destination -> List AudioProperty -> AudioNode
gainNode = AudioNode GainNode

connectTo : String -> Destination
connectTo = Connect

output : Destination
output = Output

sineWave : AudioProperty
sineWave = WaveType Sine

squareWave : AudioProperty
squareWave = WaveType Square

sawtoothWave : AudioProperty
sawtoothWave = WaveType Sawtooth

frequency : Float -> AudioProperty
frequency = Frequency

gain : Float -> AudioProperty
gain value = Gain (Scalar value)

gainValueAtTime : Float -> Float -> AudioProperty
gainValueAtTime value time = Gain (ValueAtTime value time)

gainLinearRampToValueAtTime : Float -> Float -> AudioProperty
gainLinearRampToValueAtTime value time = Gain (LinearRampToValueAtTime value time)

gainExponentialRampToValueAtTime : Float -> Float -> AudioProperty
gainExponentialRampToValueAtTime value time = Gain (ExponentialRampToValueAtTime value time)

detune : Int -> AudioProperty
detune = Detune



-- ENCODER

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

encodeWave : Wave -> Encode.Value
encodeWave wave =
  case wave of
    Sine -> Encode.string "sine"
    Square -> Encode.string "square"
    Sawtooth -> Encode.string "sawtooth"

encodeValue : Value -> Encode.Value
encodeValue value =
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

updateAudioProperty : Encode.Value -> Maybe (List Encode.Value) -> Maybe (List Encode.Value)
updateAudioProperty value existing =
  Maybe.map ((::) value) existing
    |> Maybe.withDefault [value]
    |> Just

foldAudioProperties : AudioProperty -> Dict String (List Encode.Value) -> Dict String (List Encode.Value)
foldAudioProperties property collection =
  case property of
    WaveType wave -> Dict.insert "type" [ encodeWave wave ] collection
    Frequency value -> Dict.insert "frequency" [ Encode.float value ] collection
    Gain value -> Dict.update "gain" (updateAudioProperty (encodeValue value)) collection
    Detune value -> Dict.insert "detune" [ Encode.int value ] collection

-- Temporary -- Will need to be updated to work correctly with properties that should just have a single value and properties that can have multiple
encodeAudioPropertyValue : (String, List Encode.Value) -> (String, Encode.Value)
encodeAudioPropertyValue (key, list) =
  case list of
    a::b::_ -> (key, Encode.list list)
    a::_ -> (key, a)
    [] -> (key, Encode.null)

encodeNode : AudioNode -> (String, Encode.Value)
encodeNode node =
  ( node.id
  , Encode.list
      [ encodeNodeType node.nodeType
      , encodeDestination node.destination
      , List.foldr foldAudioProperties Dict.empty node.properties |> Dict.toList |> List.map encodeAudioPropertyValue |> Encode.object
      ]
  )

encode : AudioGraph -> Encode.Value
encode graph =
  List.map encodeNode graph |> Encode.object
