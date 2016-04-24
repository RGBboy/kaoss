import Color exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Mouse
import Signal
import Signal.Extra
import Window

view : Bool -> (Int, Int) -> Html
view swap (w', h') =
  let
    (w, h) = (toString w', toString h')
    (x, y) = (toString (toFloat (w' - 320) / 2), toString (toFloat (h' - 320) / 2))
  in
    div
      [ style
          [ ("backgroundColor", "#333333")
          , ("width", w ++ "px")
          , ("height", h ++ "px")
          ]
      ]
      [ div
          [ style
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

normalize : (Int, Int) -> (Int, Int) -> (Float, Float)
normalize (w', h') (x', y') =
  let
    (w, h) = (toFloat w', toFloat h')
    (x, y) = (toFloat x', toFloat y')
  in
    (x / w, y / h)

port output : Signal (Maybe (Float, Float))
port output =
  Signal.Extra.switchWhen
    Mouse.isDown
    (Signal.map Just (Signal.map2 normalize Window.dimensions Mouse.position))
    (Signal.sampleOn Mouse.isDown (Signal.constant Nothing))


port swap : Signal Bool
