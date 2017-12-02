port module Main exposing (..)

import AudioGraph exposing (AudioGraph)
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Json.Encode as Encode
import Kaoss
import Sequencer
import Pad
import Time exposing (Time)



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
port input : (Time -> msg) -> Sub msg



-- MODEL

type State
  = Idle
  | Playing

type alias Model =
  { state : State
  -- , kaoss : Kaoss.Model
  , sequencer : Sequencer.Model
  , pad : Pad.Model
  }

init : (Model, Cmd msg)
init =
  ( { state = Idle
    -- , kaoss = Kaoss.init (320, 320)
    , sequencer = Sequencer.init
    , pad = Pad.init
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
  -- | KaossMessage Kaoss.Msg
  | SequencerMessage Sequencer.Msg
  | PadMessage Pad.Msg

update : Msg -> Model -> (Model, Cmd msg)
update message model =
  case message of
    Start ->
      let
        newModel = { model | state = Playing }
      in
      ( newModel
      , graph newModel |> AudioGraph.updateGraph |> output
      )
    -- KaossMessage msg ->
    --   let
    --     newModel = { model | kaoss = Kaoss.update msg model.kaoss }
    --   in
    --     ( newModel
    --     , graph newModel |> AudioGraph.updateGraph |> output
    --     )
    SequencerMessage msg ->
      let
        (sequencer, cmd) = Sequencer.update output msg model.sequencer
        newModel = { model | sequencer = sequencer }
        cmds =
          if newModel == model then
            cmd
          else
            Cmd.batch
                [ graph newModel |> AudioGraph.updateGraph |> output
                , cmd
                ]
      in
        ( newModel
        , cmds
        )
    PadMessage msg ->
      let
        newModel = { model | pad = Pad.update msg model.pad }
        cmd =
          if newModel == model then
            Cmd.none
          else
            graph newModel |> AudioGraph.updateGraph |> output
      in
        ( newModel
        , cmd
        )



-- GRAPH

graph : Model -> AudioGraph
graph model =
  [ AudioGraph.audioNode "0" AudioGraph.output
      <| AudioGraph.gain [ AudioGraph.value 1 ]
  ]
  |> List.append
      (Pad.graph "pad" (AudioGraph.connectTo "0") model.pad)
    -- |> List.append
    --     (Kaoss.graph "kaoss" (AudioGraph.connectTo "0") model.kaoss)
  |> List.append
      (Sequencer.graph "sequencer" (AudioGraph.connectTo "0") model.sequencer)



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  case model.state of
    Idle ->
      Sub.none
    Playing ->
      Sub.map SequencerMessage (Sequencer.subscriptions input model.sequencer)



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
      H.div
        [ A.style
            [ ("width", "100%")
            , ("height", "100%")
            ]
        ]
        [ Pad.view model.pad |> H.map PadMessage
        --, Kaoss.view model.kaoss |> H.map KaossMessage
        , Sequencer.view model.sequencer |> H.map SequencerMessage
        ]
