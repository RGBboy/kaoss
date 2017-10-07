module AudioGraph exposing
  ( AudioGraph
  , audioGraph
  , audioNode
  , oscillator, gainNode
  , connectTo, output
  , AudioProperty
  , sineWave, squareWave, sawtoothWave
  , frequency, gain, detune
  , encode
  )

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

type AudioProperty
  = WaveType Wave
  | Frequency Float
  | Gain Float
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
gain = Gain

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

encode : AudioGraph -> Encode.Value
encode graph =
  List.map encodeNode graph |> Encode.object
