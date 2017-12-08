module TouchGroup exposing
  ( Model
  , init
  , Msg
  , update
  , key
  , onTouch
  )

import Dict exposing (Dict)
import DOM
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode exposing (decode)



-- MODEL

type alias Model = Dict String Bool

init : Model
init = Dict.empty



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

collectTouched : Touches -> String -> DOM.Rectangle -> Model -> Model
collectTouched touches id target acc =
  Dict.insert id (areTouchesIntersecting touches target) acc

stopIntersecting : (String -> a -> b -> b) -> a -> String -> Bool -> (Model, b) -> (Model, b)
stopIntersecting onStop a key wasIntersecting (model, b) =
  if (False == wasIntersecting) then
    (Dict.insert key wasIntersecting model, b)
  else
    (Dict.insert key wasIntersecting model, onStop key a b)


keepIntersecting : (String -> a -> b -> b) -> (String -> a -> b -> b) -> a -> String -> Bool -> Bool -> (Model, b) -> (Model, b)
keepIntersecting onStart onStop a key wasIntersecting isIntersecting (model, b) =
  if (wasIntersecting == isIntersecting) then
    (Dict.insert key isIntersecting model, b)
  else if (False == isIntersecting) then
    (Dict.insert key isIntersecting model, onStop key a b)
  else
    (Dict.insert key isIntersecting model, onStart key a b)

startIntersecting : (String -> a -> b -> b) -> a -> String -> Bool -> (Model, b) -> (Model, b)
startIntersecting onStart a key isIntersecting (model, b) =
  if (False == isIntersecting) then
    (Dict.insert key isIntersecting model, b)
  else
    (Dict.insert key isIntersecting model, onStart key a b)

update : (String -> a -> b -> b) -> (String -> a -> b -> b) -> Msg a -> (Model, b) -> (Model, b)
update onStart onStop { children, touches, meta } (wasIntersecting, state) =
  let
    isIntersecting = Dict.foldl (collectTouched touches) Dict.empty children
  in
    Dict.merge
      (stopIntersecting onStop meta)
      (keepIntersecting onStart onStop meta)
      (startIntersecting onStart meta)
      wasIntersecting
      isIntersecting
      (Dict.empty, state)



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
    |> Decode.required "id" Decode.string
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

key : String -> H.Attribute (Msg a)
key key =
  A.id key

onTouch : Decoder a ->  List (H.Attribute (Msg a))
onTouch decoder =
  [ onTouchStart decoder
  , onTouchMove decoder
  , onTouchEnd decoder
  , onTouchCancel decoder
  ]
