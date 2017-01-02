port module Main exposing (..)

import Color exposing (..)


-- import Signal
-- import Signal.Extra
import Time
-- import Touch exposing (Touch)

import Html exposing (..)
import Html.Attributes as A
import Html.Events as Events

import Mouse
import Window


port output : Output -> Cmd msg

main : Program Never Model Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



type alias Model =
  { active: Bool
  , position: (Int, Int)
  , dimensions: (Int, Int)
  }

init : (Model, Cmd msg)
init =
  ( { active = False
    , position = (0, 0)
    , dimensions = (320, 320)
    }
  , Cmd.none
  )




type alias Output = (Bool, Float, Float)

toOutput : Model -> Output
toOutput { active, position, dimensions } =
  let
    (x, y) = normalize position dimensions
    isActive = (active && x < 1 && x > 0 && y < 1 && y > 0)
  in
    (isActive, x, y)



type Msg
  = Active Bool
  | Position Mouse.Position
  | Resize Window.Size

normalize : (Int, Int) -> (Int, Int) -> (Float, Float)
normalize (xInt, yInt) (wInt, hInt) =
  let
    (w, h) = (toFloat wInt, toFloat hInt)
    (ox, oy) = ((w - 320) / 2, (h - 320) / 2)
    (x, y) = (toFloat xInt, toFloat yInt)
  in
    ((x - ox) / 320, (y - oy) / 320)



update : Msg -> Model -> (Model, Cmd msg)
update message { active, position, dimensions } =
  case message of
    Active v ->
      let
        model = Model v position dimensions
      in
        ( model
        , output (toOutput model)
        )
    Position { x, y } ->
      let
        model = Model active (x, y) dimensions
      in
        ( model
        , output (toOutput model)
        )
    Resize { width, height } ->
      let
        model = Model active position (width, height)
      in
        ( model
        , output (toOutput model)
        )



view : Model -> Html Msg
view { dimensions } =
  let
    (width, height) = dimensions
    (w, h) = (toString width, toString height)
    (x, y) = (toString (toFloat (width - 320) / 2), toString (toFloat (height - 320) / 2))
  in
    div
      [ A.style
          [ ("backgroundColor", "#333333")
          , ("width", w ++ "px")
          , ("height", h ++ "px")
          ]
      ]
      [ div
          [ A.style
              [ ("backgroundColor", "#000000")
              , ("position", "absolute")
              , ("left", x ++ "px")
              , ("top", y ++ "px")
              , ("width", "320px")
              , ("height", "320px")
              ]
          ]
          []
      ]

-- touchToActive : (List Touch) -> Action
-- touchToActive touches =
--   case touches of
--     hd :: tl -> Active True
--     [] -> Active False
--
-- touchToPosition : (List Touch) -> Action
-- touchToPosition touches =
--   case touches of
--     hd :: tl -> Position (hd.x, hd.y)
--     [] -> Position (0, 0)





subscriptions : Model -> Sub Msg
subscriptions model =
  let
    moves =
      if model.active then
        Mouse.moves Position
      else
        Sub.none
  in
    Sub.batch
      [ Window.resizes Resize
      , Mouse.downs (always (Active True))
      , Mouse.ups (always (Active False))
      , moves
      ]

-- mouseActiveSignal : Signal Action
-- mouseActiveSignal =
--   Signal.map Active Mouse.isDown

-- mousePositionSignal : Signal Action
-- mousePositionSignal =
--   Signal.map Position Mouse.position

-- touchActiveSignal : Signal Action
-- touchActiveSignal =
--   Signal.map touchToActive Touch.touches
--
-- touchPositionSignal : Signal Action
-- touchPositionSignal =
--   Signal.map touchToPosition (Time.delay 0 Touch.touches)

-- actionSignal : Signal Action
-- actionSignal =
--   Signal.mergeMany
--     [ resizeSignal
--     , mouseActiveSignal
--     , mousePositionSignal
--     , touchPositionSignal
--     , touchActiveSignal
--     ]
