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

on : Config -> Time -> List AudioGraph.AudioParam
on { attack, decay, sustain } time =
  [ AudioGraph.linearRampToValueAtTime 0 time -- required for mobile to sound correct
  , AudioGraph.linearRampToValueAtTime 1 (time + attack)
  , AudioGraph.linearRampToValueAtTime sustain (time + attack + decay)
  ]

off : Config -> Time -> List AudioGraph.AudioParam
off { sustain, release } time =
  [ AudioGraph.linearRampToValueAtTime sustain time-- required for mobile to sound correct
  , AudioGraph.linearRampToValueAtTime 0 (time + release) -- Release
  ]
