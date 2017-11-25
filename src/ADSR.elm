module ADSR exposing
  ( Config
  , on
  , off
  )

import AudioGraph exposing (AudioGraph)
import Time exposing (Time)


type alias Config =
  { attack : Time
  , decay : Time
  , sustain : Float
  , release : Time
  }

on : Config -> Time -> List AudioGraph.AudioProperty
on { attack, decay, sustain } time =
  [ AudioGraph.gainLinearRampToValueAtTime 0 time -- required for mobile to sound correct
  , AudioGraph.gainLinearRampToValueAtTime 1 (time + attack)
  , AudioGraph.gainLinearRampToValueAtTime sustain (time + attack + decay)
  ]

off : Config -> Time -> List AudioGraph.AudioProperty
off { sustain, release } time =
  [ AudioGraph.gainLinearRampToValueAtTime sustain time-- required for mobile to sound correct
  , AudioGraph.gainLinearRampToValueAtTime 0 (time + release) -- Release
  ]
