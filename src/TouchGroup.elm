module TouchGroup exposing
  ( Model
  , init
  , Msg
  , update
  , view
  )

import Color exposing (Color)
import Dict exposing (Dict)
import DOM
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode exposing (decode)



-- MODEL

type alias TouchItem a = (Bool, a)

type alias Model a = Dict String (TouchItem a)

init : Dict String a -> Model a
init = Dict.map (\ _ value -> (False, value))



-- UPDATE

type alias Touches
  = List (Float, Float)

type alias TouchEvent a =
  { children : Dict String DOM.Rectangle
  , touches : Touches
  , meta : a
  }

type alias Msg a = TouchEvent a

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

updateItem : (b -> b) -> Touches -> DOM.Rectangle -> (Bool, b) -> (Bool, b)
updateItem fn touches target (wasIntersecting, oldState) =
  let
    isIntersecting = areTouchesIntersecting touches target
  in
    if (wasIntersecting == isIntersecting) then
      (wasIntersecting, oldState)
    else
      (isIntersecting, fn oldState)

mergeBoth : (a -> b -> c) -> comparable -> a -> b -> Dict comparable c -> Dict comparable c
mergeBoth fn id left right result =
  Dict.insert id (fn left right) result

update : (a -> b -> b) -> Msg a -> Dict String (Bool, b) -> Dict String (Bool, b)
update fn { children, touches, meta } model =
  let
      updateState = fn meta
      updateItemState = updateItem updateState touches
  in
    Dict.merge
      (\ id target acc -> acc)
      (mergeBoth updateItemState)
      (\ id touchItem acc -> acc)
      children
      model
      model



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

decodeNode : Decoder (String, DOM.Rectangle)
decodeNode =
  decode (,)
    |> Decode.custom DOM.className
    |> Decode.custom DOM.boundingClientRect

decodeNodes : Decoder (Dict String DOM.Rectangle)
decodeNodes =
  decode Dict.fromList
    |> Decode.custom (DOM.childNodes decodeNode)

decodeTouchEvent : Decoder a -> Decoder (TouchEvent a)
decodeTouchEvent decoder =
  decode TouchEvent
    |> Decode.required "currentTarget" decodeNodes
    |> Decode.required "touches" decodeTouchList
    |> Decode.custom decoder

options : E.Options
options =
  { stopPropagation = True
  , preventDefault = True
  }

onTouchStart : Decoder a -> H.Attribute (Msg a)
onTouchStart decoder =
  E.onWithOptions "touchstart" options (decodeTouchEvent decoder)

onTouchMove : Decoder a -> H.Attribute (Msg a)
onTouchMove decoder =
  E.onWithOptions "touchmove" options (decodeTouchEvent decoder)

onTouchEnd : Decoder a -> H.Attribute (Msg a)
onTouchEnd decoder =
  E.onWithOptions "touchend" options (decodeTouchEvent decoder)

onTouchCancel : Decoder a -> H.Attribute (Msg a)
onTouchCancel decoder =
  E.onWithOptions "touchcancel" options (decodeTouchEvent decoder)

itemView : String -> (Bool, a) -> Html msg
itemView key (isActive, _) =
  let
    color =
      if isActive == True then
        "#999999"
      else
        "#666666"
  in
    H.div
      [ A.class key -- change this later to be a data attribute
      , A.style
          [ ("backgroundColor", color)
          , ("box-sizing", "border-box")
          , ("border", "4px solid #333333")
          , ("width", "50%")
          , ("height", "25%")
          , ("float", "left")
          ]
      ]
      []

view : Decoder a -> Model b -> Html (Msg a)
view decoder model =
  H.div
    [ A.style
        [ ("backgroundColor", "#333333")
        , ("width", "100%")
        , ("height", "100%")
        ]
    , onTouchStart decoder
    , onTouchMove decoder
    , onTouchEnd decoder
    , onTouchCancel decoder
    ]
    (Dict.map itemView model |> Dict.values)
