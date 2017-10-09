port module Main exposing (..)

import AudioGraph exposing (AudioGraph)
import Html as H exposing (Html)
import Html.Events as E
import Json.Encode as Encode
import Kaoss



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
  , kaoss : Kaoss.Model
  }

init : (Model, Cmd msg)
init =
  ( { state = Idle
    , kaoss = Kaoss.init (320, 320)
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

type Msg
  = Start
  | KaossMessage Kaoss.Msg

update : Msg -> Model -> (Model, Cmd msg)
update message model =
  case message of
    Start ->
      ( { model | state = Playing }
      , outputType "init" Encode.null |> output
      )
    KaossMessage msg ->
      let
        newModel = { model | kaoss = Kaoss.update msg model.kaoss }
      in
        ( newModel
        , graph newModel |> AudioGraph.encode |> outputType "update" |> output
        )



-- GRAPH

graph : Model -> AudioGraph
graph model =
  Kaoss.graph "kaoss" (AudioGraph.connectTo "0") model.kaoss
    |> List.append
        [ AudioGraph.gainNode "0" AudioGraph.output
            [ AudioGraph.gain 1 ]
        ]



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW

view : Model -> Html Msg
view model =
  case model.state of
    Idle ->
      H.button
        [ E.onClick Start
        ]
        [ H.text "Start" ]
    _ ->
      Kaoss.view model.kaoss |> H.map KaossMessage
