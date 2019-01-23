module Phoenix exposing (update)

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
            ( { socket | hasErrored = True, isConnected = False }, maybeTriggerCmdWithPayload socket.onError payload )

        Incoming SocketOpened ->
            ( { socket | isConnected = True }, maybeTriggerCommand socket.onOpen )

        _ ->
            ( socket, Cmd.none )


maybeTriggerCommand : Maybe msg -> Cmd msg
maybeTriggerCommand maybeCallback =
    case maybeCallback of
        Just msg_ ->
            Task.perform identity <| Task.succeed msg_

        Nothing ->
            Cmd.none


maybeTriggerCmdWithPayload : Maybe (Payload -> msg) -> (Payload -> Cmd msg)
maybeTriggerCmdWithPayload maybeCallback =
    case maybeCallback of
        Just fn ->
            Task.perform identity << Task.succeed << fn

        Nothing ->
            \_ -> Cmd.none
