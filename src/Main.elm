import Color exposing (..)
import Html exposing (..)
import Html.Attributes as A
import Html.Events as Events
import Mouse
import Signal
import Signal.Extra
import Time
import Touch exposing (Touch)
import Window

import Debug

type alias Model =
  { active: Bool
  , position: (Int, Int)
  , dimensions: (Int, Int)
  }

init : Model
init =
  { active = False
  , position = (0, 0)
  , dimensions = (320, 320)
  }

type alias Output = (Bool, Float, Float)

toOutput : Model -> Output
toOutput { active, position, dimensions } =
  let
    (x, y) = normalize position dimensions
    active' = (active && x < 1 && x > 0 && y < 1 && y > 0)
  in
    (active', x, y)

type Action
  = Active Bool
  | Position (Int, Int)
  | Resize (Int, Int)

normalize : (Int, Int) -> (Int, Int) -> (Float, Float)
normalize (x', y') (w', h') =
  let
    (w, h) = (toFloat w', toFloat h')
    (ox, oy) = ((w - 320) / 2, (h - 320) / 2)
    (x, y) = (toFloat x', toFloat y')
  in
    ((x - ox) / 320, (y - oy) / 320)

update : Action -> Model -> Model
update action { active, position, dimensions } =
  case action of
    Active v ->
      Model v position dimensions
    Position v ->
      Model active v dimensions
    Resize v ->
      Model active position v

view : Bool -> (Int, Int) -> Html
view swap (w', h') =
  let
    (w, h) = (toString w', toString h')
    (x, y) = (toString (toFloat (w' - 320) / 2), toString (toFloat (h' - 320) / 2))
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

touchToActive : (List Touch) -> Action
touchToActive touches =
  case touches of
    hd :: tl -> Active True
    [] -> Active False

touchToPosition : (List Touch) -> Action
touchToPosition touches =
  case touches of
    hd :: tl -> Position (hd.x, hd.y)
    [] -> Position (0, 0)

main : Signal Html
main =
  Signal.map2 view swap Window.dimensions

resizeSignal : Signal Action
resizeSignal =
  Signal.map Resize Window.dimensions

mouseActiveSignal : Signal Action
mouseActiveSignal =
  Signal.map Active Mouse.isDown

mousePositionSignal : Signal Action
mousePositionSignal =
  Signal.map Position Mouse.position

touchActiveSignal : Signal Action
touchActiveSignal =
  Signal.map touchToActive Touch.touches

touchPositionSignal : Signal Action
touchPositionSignal =
  Signal.map touchToPosition (Time.delay 0 Touch.touches)

actionSignal : Signal Action
actionSignal =
  Signal.mergeMany
    [ resizeSignal
    , mouseActiveSignal
    , mousePositionSignal
    , touchPositionSignal
    , touchActiveSignal
    ]

loop : Signal Model
loop =
  Signal.Extra.foldp' update (\action -> update action init) actionSignal

port output : Signal Output
port output =
  Signal.map toOutput loop

port swap : Signal Bool
