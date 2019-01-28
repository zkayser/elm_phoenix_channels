module Phoenix exposing (update)

import Dict
import Json.Encode as Encode exposing (Value)
import Phoenix.Message as Message exposing (Event(..), Message(..))
import Phoenix.Payload exposing (Payload)
import Phoenix.Socket as Socket exposing (Socket)
import Task


update : Message msg -> Socket msg -> ( Socket msg, Cmd msg )
update phoenixMessage socket =
    case phoenixMessage of
        Incoming SocketClosed ->
            ( { socket | hasClosed = True, isConnected = False }, maybeTriggerCommand socket.onClose )

        Incoming (SocketErrored payload) ->
            ( { socket | hasErrored = True, isConnected = False }, maybeTriggerCmdWithPayload socket.onError payload.payload )

        Incoming SocketOpened ->
            ( { socket | isConnected = True }, maybeTriggerCommand socket.onOpen )

        Incoming (ChannelJoined payload) ->
            case Dict.get payload.topic socket.channels of
                Just channel -> ( socket, maybeTriggerCmdWithPayload channel.onJoin payload.payload )
                _ -> ( socket, Cmd.none )
        Incoming (ChannelJoinError payload) ->
            case Dict.get payload.topic socket.channels of
                Just channel -> ( socket, maybeTriggerCmdWithPayload channel.onJoinError payload.payload )
                _ -> ( socket, Cmd.none )
        Incoming (ChannelJoinTimeout payload) ->
            case Dict.get payload.topic socket.channels of
                Just channel -> ( socket, maybeTriggerCommand channel.onJoinTimeout )
                _ -> ( socket, Cmd.none )
        Incoming (ChannelMessageReceived payload) -> 
            case Dict.get payload.topic socket.channels of
                Just channel -> ( socket, maybeTriggerCmdWithPayload (Dict.get payload.message channel.on) payload.payload )
                _ -> ( socket, Cmd.none )
        Incoming (ChannelLeft payload) -> 
            case Dict.get payload.topic socket.channels of
                Just channel -> ( socket, maybeTriggerCmdWithPayload channel.onLeave payload.payload )
                _ -> ( socket, Cmd.none )
        Incoming (ChannelLeaveError payload) -> 
            case Dict.get payload.topic socket.channels of
                Just channel -> ( socket, maybeTriggerCmdWithPayload channel.onLeaveError payload.payload )
                _ -> ( socket, Cmd.none)
        Incoming (PushOk payload) ->
            case Dict.get payload.topic socket.pushes of
                Just push -> ( socket, maybeTriggerCmdWithPayload push.onOk payload.payload )
                _ -> ( socket, Cmd.none )
        Incoming (PushError payload) ->
            case Dict.get payload.topic socket.pushes of
                Just push -> ( socket, maybeTriggerCmdWithPayload push.onError payload.payload )
                _ -> ( socket, Cmd.none )
        _ ->
            ( socket, Cmd.none )


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
