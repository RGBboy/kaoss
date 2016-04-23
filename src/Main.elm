import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Mouse
import Signal
import Signal.Extra
import Window

view : Bool -> (Int, Int) -> Element
view swap (w',h') =
  let
    (w,h) = (toFloat w', toFloat h')
  in
    collage w' h'
      [ rect w h
          |> filled (rgb 32 32 32)
      , rect 320 320
          |> filled (rgb 0 0 0)
      ]

main : Signal Element
main =
  Signal.map2 view swap Window.dimensions

normalize : (Int, Int) -> (Int, Int) -> (Float, Float)
normalize (w', h') (x', y') =
  let
    (w, h) = (toFloat w', toFloat h')
    (x, y) = (toFloat x', toFloat y')
  in
    (x / w, y / h)

port output : Signal (Float, Float)
port output =
  -- Signal.Extra.keepWhen Mouse.isDown (0,0)
    (Signal.map2 normalize Window.dimensions Mouse.position)

port swap : Signal Bool
