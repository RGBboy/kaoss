import Color exposing (..)
import Html exposing (..)
import Html.Attributes as A
import Html.Events as Events
import Mouse
import Signal
import Window

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
    (x, y) = normalize dimensions position
    active' = (active && x < 1 && x > 0 && y < 1 && y > 0)
  in
    (active', x, y)

type Action
  = Resize (Int, Int)
  | Active Bool
  | Position (Int, Int)

normalize : (Int, Int) -> (Int, Int) -> (Float, Float)
normalize (w', h') (x', y') =
  let
    (w, h) = (toFloat w', toFloat h')
    (ox, oy) = ((w - 320) / 2, (h - 320) / 2)
    (x, y) = (toFloat x', toFloat y')
  in
    ((x - ox) / 320, (y - oy) / 320)

update : Action -> Model -> Model
update action { active, position, dimensions } =
  case action of
    Resize v -> Model active position v
    Active v -> Model v position dimensions
    Position v -> Model active v dimensions

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

main : Signal Html
main =
  Signal.map2 view swap Window.dimensions

resizeSignal : Signal Action
resizeSignal =
  Signal.map Resize Window.dimensions

activeSignal : Signal Action
activeSignal =
  Signal.map Active Mouse.isDown

positionSignal : Signal Action
positionSignal =
  Signal.map Position Mouse.position

actionSignal : Signal Action
actionSignal =
  Signal.mergeMany
    [ resizeSignal
    , activeSignal
    , positionSignal
    ]

loop : Signal Model
loop =
  Signal.foldp update init actionSignal

port output : Signal Output
port output =
  Signal.map toOutput loop

port swap : Signal Bool
