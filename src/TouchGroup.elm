module TouchGroup exposing
  ( Model
  , init
  , Msg
  , update
  , view
  )

import Color exposing (Color)
import DOM
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode exposing (decode)



-- MODEL

type alias Model = List Bool

init : Model
init = [False, False, False, False]



-- UPDATE

type alias Touches
  = List (Float, Float)

type alias TouchEvent =
  { children : List DOM.Rectangle
  , touches : Touches
  }

type alias Msg = TouchEvent

isTouchIntersecting : DOM.Rectangle -> (Float, Float) -> Bool -> Bool
isTouchIntersecting { top, left, width, height } (x, y) existing =
  if
    y > top
    && y < (top + height)
    && x > left
    && x < (left + width)
    || existing
  then
    True
  else
    False

areTouchesIntersecting : Touches -> DOM.Rectangle -> Bool
areTouchesIntersecting touches target =
  List.foldl (isTouchIntersecting target) False touches

update : Msg -> Model -> Model
update { children, touches } _ =
  List.map (areTouchesIntersecting touches) children



-- VIEW

decodeAll : List (Decoder a) -> Decoder (List a)
decodeAll =
  List.foldr (Decode.map2 (::)) (Decode.succeed [])

decodeTouch : Decoder (Float, Float)
decodeTouch =
  decode (,)
    |> Decode.required "pageX" Decode.float
    |> Decode.required "pageY" Decode.float

decodeTouchAt : Int -> Decoder (Float, Float)
decodeTouchAt n =
    Decode.field (toString n) decodeTouch

decodeTouches : Int -> Decoder Touches
decodeTouches nbTouches =
    List.range 0 (nbTouches - 1)
      |> List.map decodeTouchAt
      |> decodeAll

decodeTouchList : Decoder Touches
decodeTouchList =
  Decode.field "length" Decode.int
    |> Decode.andThen decodeTouches

decodeTouchEvent : Decoder TouchEvent
decodeTouchEvent =
  decode TouchEvent
    |> Decode.required "currentTarget" (DOM.childNodes DOM.boundingClientRect)
    |> Decode.required "touches" decodeTouchList

options : E.Options
options =
  { stopPropagation = True
  , preventDefault = True
  }

onTouchStart : (TouchEvent -> msg) -> H.Attribute msg
onTouchStart tagger =
  E.onWithOptions "touchstart" options (Decode.map ((Debug.log "onTouchStart") >> tagger) decodeTouchEvent)

onTouchMove : (TouchEvent -> msg) -> H.Attribute msg
onTouchMove tagger =
  E.onWithOptions "touchmove" options (Decode.map ((Debug.log "onTouchMove") >> tagger) decodeTouchEvent)

onTouchEnd : (TouchEvent -> msg) -> H.Attribute msg
onTouchEnd tagger =
  E.onWithOptions "touchend" options (Decode.map ((Debug.log "onTouchEnd") >> tagger) decodeTouchEvent)

onTouchCancel : (TouchEvent -> msg) -> H.Attribute msg
onTouchCancel tagger =
  E.onWithOptions "touchcancel" options (Decode.map ((Debug.log "onTouchCancel") >> tagger) decodeTouchEvent)

itemView : Bool -> Html msg
itemView isActive =
  let
    color =
      if isActive == True then
        "#999999"
      else
        "#666666"
  in
    H.div
      [ A.style
          [ ("backgroundColor", color)
          , ("box-sizing", "border-box")
          , ("border", "4px solid #333333")
          , ("width", "50%")
          , ("height", "50%")
          , ("float", "left")
          ]
      ]
      []

view : Model -> Html Msg
view model =
  H.div
    [ A.style
        [ ("backgroundColor", "#333333")
        , ("margin-left", "25%")
        , ("margin-top", "25%")
        , ("width", "50%")
        , ("height", "500px")
        ]
    , onTouchStart identity
    , onTouchMove identity
    , onTouchEnd identity
    , onTouchCancel identity
    ]
    <| List.map itemView model
