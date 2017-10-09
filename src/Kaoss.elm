module Kaoss exposing
  ( Model
  , init
  , Msg
  , update
  , graph
  , view
  )

import AudioGraph exposing (AudioGraph)
import Color exposing (Color)
import Html as H exposing (Html)
import Html.Attributes as A
import MouseEvents as Mouse
import SingleTouch
import Touch



-- MODEL

type alias Model =
  { position : Maybe (Float, Float)
  , dimensions : (Float, Float)
  }

init : (Float, Float) -> Model
init dimensions =
  { position = Nothing
  , dimensions = dimensions
  }



-- UPDATE

type Msg
  = Start (Float, Float)
  | Position (Float, Float)
  | Stop

update : Msg -> Model -> Model
update message ({ position, dimensions } as model) =
  case message of
    Start value ->
      { model | position = Just value }
    Position value ->
      { model | position = Maybe.map (always value) position }
    Stop ->
      { model | position = Nothing }



-- GRAPH

normalize : (Float, Float) -> (Float, Float) -> (Float, Float)
normalize (bX, bY) (aX, aY) =
  (aX / bX, aY / bY)

clamp : (number, number) -> (number, number) -> (number, number) -> (number, number)
clamp (minX, minY) (maxX, maxY) (x, y) =
  (Basics.clamp minX maxX x, Basics.clamp minY maxY y)

frequencyRatio : Float -> Float
frequencyRatio value =
  (2 ^ value) ^ (1 / 12)

createGraph : String -> AudioGraph.Destination -> Float -> Float -> AudioGraph
createGraph id output g f =
  let
    rootId = id ++ "-0"
  in
    [ AudioGraph.gainNode rootId output
        [ AudioGraph.gain (g * g * 0.5) ]
    , AudioGraph.oscillator (id ++ "-1") (AudioGraph.connectTo rootId)
        [ AudioGraph.squareWave
        , frequencyRatio f |> (*) 110 |> AudioGraph.frequency
        ]
    , AudioGraph.oscillator (id ++ "-2") (AudioGraph.connectTo rootId)
        [ AudioGraph.squareWave
        , f + 7 |> frequencyRatio |> (*) 110 |> AudioGraph.frequency
        , AudioGraph.detune 4
        ]
    ]

graph : String -> AudioGraph.Destination -> Model -> AudioGraph
graph id output { dimensions, position } =
  Maybe.map (normalize dimensions) position
    |> Maybe.withDefault (0, 0)
    |> clamp (0, 0) (1, 1)
    |> (uncurry (createGraph id output))



-- VIEW

tupleFromXY : { x : number, y : number } -> (number, number)
tupleFromXY { x, y } =
  (x, y)

intToFloat : (Int, Int) -> (Float, Float)
intToFloat (x, y) =
  (toFloat x, toFloat y)

view : Model -> Html Msg
view { dimensions, position } =
  let
    (width, height) = dimensions
    (w, h) = (toString width, toString height)
    color = Maybe.map (always "#333333") position
      |> Maybe.withDefault "#000000"
  in
    H.div
      [ A.style
          [ ("backgroundColor", "#333333")
          , ("width", w ++ "px")
          , ("height", h ++ "px")
          , ("margin", "0px auto")
          ]
      ]
      [ H.div
          [ A.style
              [ ("backgroundColor", color)
              , ("width", "100%")
              , ("height", "100%")
              ]
          , Mouse.onMouseDown (Mouse.relPos >> tupleFromXY >> intToFloat >> Start)
          , Mouse.onMouseUp (always Stop)
          , Mouse.onMouseOut (always Stop)
          , Mouse.onMouseMove (Mouse.relPos >> tupleFromXY >> intToFloat >> Position)
          , SingleTouch.onStart (Touch.clientPos >> Start)
          , SingleTouch.onEnd (always Stop)
          , SingleTouch.onCancel (always Stop)
          , SingleTouch.onMove (Touch.clientPos >> Position)
          ]
          []
      ]
