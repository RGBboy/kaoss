port module Main exposing (..)

import Color exposing (Color)




import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as Events
import Mouse
import Time
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

port output : Output -> Cmd msg

type alias Output = (Bool, Float, Float)

toOutput : Model -> Output
toOutput { active, position, dimensions } =
  let
    (x, y) = normalize position dimensions
    isActive = (active && x < 1 && x > 0 && y < 1 && y > 0)
  in
    (isActive, x, y)



-- MODEL

type alias Model =
  { active : Bool
  , position : (Int, Int)
  , dimensions : (Int, Int)
  }

init : (Model, Cmd msg)
init =
  ( { active = False
    , position = (0, 0)
    , dimensions = (320, 320)
    }
  , Cmd.none
  )



-- UPDATE

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



-- SUBSCRIPTIONS

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



-- VIEW

view : Model -> Html Msg
view { dimensions } =
  let
    (width, height) = dimensions
    (w, h) = (toString width, toString height)
    (x, y) = (toString (toFloat (width - 320) / 2), toString (toFloat (height - 320) / 2))
  in
    H.div
      [ A.style
          [ ("backgroundColor", "#333333")
          , ("width", w ++ "px")
          , ("height", h ++ "px")
          ]
      ]
      [ H.div
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
