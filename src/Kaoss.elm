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
import Json.Encode as Encode
import MouseEvents as Mouse
import SingleTouch
import Time
import Touch
import Window



-- MODEL

type alias Model =
  { isActive : Bool
  , position : (Float, Float)
  , dimensions : (Float, Float)
  }

init : (Float, Float) -> Model
init dimensions =
  { isActive = False
  , position = (0, 0)
  , dimensions = dimensions
  }



-- UPDATE

-- Can remodel this to remove active and have Maybe (Float, Float) for position
type Msg
  = Active Bool
  | Position (Float, Float)

update : Msg -> Model -> Model
update message ({ isActive, position, dimensions } as model) =
  case message of
    Active isActive ->
      { model | isActive = isActive }
    Position position ->
      { model | position = position }



-- GRAPH

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
graph id output { isActive, dimensions, position } =
  normalize position dimensions
    |> clamp (0, 0) (1, 1)
    |> scale (boolToFloat isActive)
    |> (uncurry (createGraph id output))



-- VIEW

tupleFromXY : { x : number, y : number } -> (number, number)
tupleFromXY { x, y } =
  (x, y)

intToFloat : (Int, Int) -> (Float, Float)
intToFloat (x, y) =
  (toFloat x, toFloat y)

view : Model -> Html Msg
view { isActive, dimensions } =
  let
    (width, height) = dimensions
    (w, h) = (toString width, toString height)
    color =
      case isActive of
        True -> "#333333"
        False -> "#000000"
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
          , Mouse.onMouseDown (always <| Active True)
          , Mouse.onMouseUp (always <| Active False)
          , Mouse.onMouseOut (always <| Active False)
          , Mouse.onMouseMove (Mouse.relPos >> tupleFromXY >> intToFloat >> Position)
          , SingleTouch.onStart (always (Active True))
          , SingleTouch.onEnd (always (Active False))
          , SingleTouch.onCancel (always (Active False))
          , SingleTouch.onMove (Touch.clientPos >> Position)
          ]
          []
      ]
