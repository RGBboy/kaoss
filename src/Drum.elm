module Drum exposing
  ( kick808
  , hihat808
  )

import AudioGraph exposing (AudioGraph)
import Time exposing (Time)

kick808 : Float -> String -> AudioGraph.Destination -> Time -> AudioGraph
kick808 fundamental id output time =
  let
    gainId = id ++ "-gain"
    oscId = id ++ "-osc"
  in
    [ AudioGraph.audioNode gainId output
        <| AudioGraph.gain
            [ AudioGraph.valueAtTime 0 time
            , AudioGraph.valueAtTime 0 (time + 0.02)
            , AudioGraph.linearRampToValueAtTime 1 (time + 0.025)
            , AudioGraph.linearRampToValueAtTime 0 (time + 0.125)
            ]
    , AudioGraph.audioNode oscId (AudioGraph.connectTo gainId)
        <| AudioGraph.sineWave
            [ AudioGraph.valueAtTime (fundamental * 2) (time + 0.02)
            , AudioGraph.exponentialRampToValueAtTime fundamental (time + 0.15)
            ]
            0
    ]

hihat808 : Float -> String -> AudioGraph.Destination -> Time -> AudioGraph
hihat808 fundamental id output time =
  let
    gainId = id ++ "-gain"
    hipassId = id ++ "-highpass"
    bandpassId = id ++ "-bandpass"
    osc0Id = id ++ "-osc-0"
    osc1Id = id ++ "-osc-1"
    osc2Id = id ++ "-osc-2"
    osc3Id = id ++ "-osc-3"
    osc4Id = id ++ "-osc-4"
    osc5Id = id ++ "-osc-5"
  in
    [ AudioGraph.audioNode gainId output
        <| AudioGraph.gain
            [ AudioGraph.valueAtTime 0 time
            , AudioGraph.exponentialRampToValueAtTime 1 (time + 0.02)
            , AudioGraph.exponentialRampToValueAtTime 0.3 (time + 0.03)
            , AudioGraph.exponentialRampToValueAtTime 0.00001 (time + 0.3)
            ]
    , AudioGraph.audioNode hipassId (AudioGraph.connectTo gainId)
        <| AudioGraph.highPassFilter 7000
    , AudioGraph.audioNode bandpassId (AudioGraph.connectTo hipassId)
        <| AudioGraph.bandPassFilter 10000
    , AudioGraph.audioNode osc0Id (AudioGraph.connectTo bandpassId)
        <| AudioGraph.squareWave [ AudioGraph.value (fundamental * 2) ] 0
    , AudioGraph.audioNode osc1Id (AudioGraph.connectTo bandpassId)
        <| AudioGraph.squareWave [ AudioGraph.value (fundamental * 3) ] 0
    , AudioGraph.audioNode osc2Id (AudioGraph.connectTo bandpassId)
        <| AudioGraph.squareWave [ AudioGraph.value (fundamental * 4.16) ] 0
    , AudioGraph.audioNode osc3Id (AudioGraph.connectTo bandpassId)
        <| AudioGraph.squareWave [ AudioGraph.value (fundamental * 5.43) ] 0
    , AudioGraph.audioNode osc4Id (AudioGraph.connectTo bandpassId)
        <| AudioGraph.squareWave [ AudioGraph.value (fundamental * 6.79) ] 0
    , AudioGraph.audioNode osc5Id (AudioGraph.connectTo bandpassId)
        <| AudioGraph.squareWave [ AudioGraph.value (fundamental * 8.21) ] 0
    ]
