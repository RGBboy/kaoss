port module Main exposing (..)

import AudioGraph exposing (AudioGraph)
import Color exposing (Color)
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Json.Encode as Encode
import MouseEvents as Mouse
import SingleTouch
import Time
import Touch
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

port output : Encode.Value -> Cmd msg

-- MODEL

type State
  = Idle
  | Playing

type alias Model =
  { state : State
  , isActive : Bool
  , position : (Float, Float)
  , dimensions : (Float, Float)
  , graph : AudioGraph
  }

init : (Model, Cmd msg)
init =
  ( { state = Idle
    , isActive = False
    , position = (0, 0)
    , dimensions = (320, 320)
    , graph = []
    }
  , Cmd.none
  )

outputType : String -> Encode.Value -> Encode.Value
outputType kind data =
  [ ("type", Encode.string kind)
  , ("data", data)
  ]
    |> Encode.object



-- UPDATE

-- Can remodel this to remove active and have Maybe (Float, Float) for position
type Msg
  = Start
  | Active Bool
  | Position (Float, Float)
  | Resize Window.Size

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

graph : Float -> Float -> AudioGraph
graph g f =
  [ AudioGraph.gainNode "0" AudioGraph.output
      [ AudioGraph.gain (g * g * 0.5) ]
  , AudioGraph.oscillator "1" (AudioGraph.connectTo "0")
      [ AudioGraph.squareWave
      , frequencyRatio f |> (*) 110 |> AudioGraph.frequency
      ]
  , AudioGraph.oscillator "2" (AudioGraph.connectTo "0")
      [ AudioGraph.squareWave
      , f + 7 |> frequencyRatio |> (*) 110 |> AudioGraph.frequency
      , AudioGraph.detune 4
      ]
  ]

createGraph : Bool -> (Float, Float) -> (Float, Float) -> AudioGraph
createGraph isActive dimensions position =
  normalize position dimensions
    |> clamp (0, 0) (1, 1)
    |> scale (boolToFloat isActive)
    |> (uncurry graph)

update : Msg -> Model -> (Model, Cmd msg)
update message ({ isActive, position, dimensions } as model) =
  case message of
    Start ->
      ( { model | state = Playing }
      , outputType "init" Encode.null |> output
      )
    Active isActive ->
      let
        newModel =
          { model
          | isActive = isActive
          , graph = createGraph isActive model.dimensions model.position
          }
      in
        ( newModel
        , AudioGraph.encode newModel.graph |> outputType "update" |> output
        )
    Position position ->
      let
        newModel =
          { model
          | position = position
          , graph = createGraph model.isActive model.dimensions position
          }
      in
        ( newModel
        , AudioGraph.encode newModel.graph |> outputType "update" |> output
        )
    Resize { width, height } ->
      let
        dimensions = (toFloat width, toFloat height)
        newModel =
          { model
          | dimensions = dimensions
          , graph = createGraph model.isActive dimensions model.position
          }
      in
        ( newModel
        , AudioGraph.encode newModel.graph |> outputType "update" |> output
        )



-- SUBSCRIPTIONS

tupleFromXY : { x : number, y : number } -> (number, number)
tupleFromXY { x, y } =
  (x, y)

intToFloat : (Int, Int) -> (Float, Float)
intToFloat (x, y) =
  (toFloat x, toFloat y)

subscriptions : Model -> Sub Msg
subscriptions model =
  Window.resizes Resize



-- VIEW

view : Model -> Html Msg
view { state, isActive, dimensions } =
  let
    (width, height) = dimensions
    (w, h) = (toString width, toString height)
    color =
      case isActive of
        True -> "#333333"
        False -> "#000000"
  in
    case state of
      Idle ->
        H.button
          [ E.onClick Start
          ]
          [ H.text "Start" ]
      _ ->
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
