module Phoenix exposing (Model, initialize, update, addChannel, addPush)

import Dict exposing (Dict)
import Json.Encode as Encode exposing (Value)
import Phoenix.Channel exposing (Channel)
import Phoenix.Message as Message exposing (Data, Event(..), Message(..))
import Phoenix.Payload exposing (Payload)
import Phoenix.Push exposing (Push)
import Phoenix.Socket as Socket exposing (Socket)
import Task


type alias Model msg =
    { socket : Socket msg
    , channels : Dict String (Channel msg)
    , pushes : Dict String (Push msg)
    , send : Data -> Cmd msg
    }


initialize : Socket msg -> (Data -> Cmd msg) -> Model msg
initialize socket sendFn =
    { socket = socket
    , channels = Dict.empty
    , pushes = Dict.empty
    , send = sendFn
    }

addChannel : Channel msg -> Model msg -> Model msg
addChannel channel model =
    { model | channels = Dict.insert channel.topic channel model.channels }

addPush : Push msg -> Model msg -> Model msg
addPush push model =
    { model | pushes = Dict.insert push.topic push model.pushes }


update : Message msg -> Model msg -> ( Model msg, Cmd msg )
update phoenixMessage model =
    let
        socket =
            model.socket
    in
    case phoenixMessage of
        Incoming SocketClosed ->
            ( { model | socket = Socket.close socket }, maybeTriggerCommand socket.onClose )

        Incoming (SocketErrored payload) ->
            ( { model | socket = Socket.errored socket }, maybeTriggerCmdWithPayload socket.onError payload.payload )

        Incoming SocketOpened ->
            ( { model | socket = Socket.opened socket }, maybeTriggerCommand socket.onOpen )

        Incoming (ChannelJoined payload) ->
            case Dict.get payload.topic model.channels of
                Just channel ->
                    ( model, maybeTriggerCmdWithPayload channel.onJoin payload.payload )

                _ ->
                    ( model, Cmd.none )

        Incoming (ChannelJoinError payload) ->
            case Dict.get payload.topic model.channels of
                Just channel ->
                    ( model, maybeTriggerCmdWithPayload channel.onJoinError payload.payload )

                _ ->
                    ( model, Cmd.none )

        Incoming (ChannelJoinTimeout payload) ->
            case Dict.get payload.topic model.channels of
                Just channel ->
                    ( model, maybeTriggerCommand channel.onJoinTimeout )

                _ ->
                    ( model, Cmd.none )

        Incoming (ChannelMessageReceived payload) ->
            case Dict.get payload.topic model.channels of
                Just channel ->
                    ( model, maybeTriggerCmdWithPayload (Dict.get payload.message channel.on) payload.payload )

                _ ->
                    ( model, Cmd.none )

        Incoming (ChannelLeft payload) ->
            case Dict.get payload.topic model.channels of
                Just channel ->
                    ( model, maybeTriggerCmdWithPayload channel.onLeave payload.payload )

                _ ->
                    ( model, Cmd.none )

        Incoming (ChannelLeaveError payload) ->
            case Dict.get payload.topic model.channels of
                Just channel ->
                    ( model, maybeTriggerCmdWithPayload channel.onLeaveError payload.payload )

                _ ->
                    ( model, Cmd.none )

        Incoming (PushOk payload) ->
            case Dict.get payload.topic model.pushes of
                Just push ->
                    ( model, maybeTriggerCmdWithPayload push.onOk payload.payload )

                _ ->
                    ( model, Cmd.none )

        Incoming (PushError payload) ->
            case Dict.get payload.topic model.pushes of
                Just push ->
                    ( model, maybeTriggerCmdWithPayload push.onError payload.payload )

                _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


maybeTriggerCommand : Maybe msg -> Cmd msg
maybeTriggerCommand maybeCallback =
    case maybeCallback of
        Just msg_ ->
            Task.perform identity <| Task.succeed msg_

        Nothing ->
            Cmd.none


maybeTriggerCmdWithPayload : Maybe (Value -> msg) -> (Value -> Cmd msg)
maybeTriggerCmdWithPayload maybeCallback =
    case maybeCallback of
        Just fn ->
            Task.perform identity << Task.succeed << fn

        Nothing ->
            \_ -> Cmd.none
