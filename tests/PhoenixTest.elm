module PhoenixTest exposing (suite)

import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode as Decode
import Json.Encode as Encode
import Phoenix
import Phoenix.Channel as Channel exposing (Channel)
import Phoenix.Message exposing (Event(..), Message(..))
import Phoenix.Push as Push
import Phoenix.Socket as Socket
import Task
import Test exposing (..)


suite : Test
suite =
    describe "Phoenix"
        [ describe "update"
            [ describe "Incoming" <|
                let
                    initSocket =
                        Socket.init "/socket"

                    initModel =
                        Phoenix.initialize initSocket fakeSend
                in
                [ describe "SocketClosed"
                    [ test "sets socket state to closed" <|
                        \_ ->
                            let
                                ( newModel, _ ) =
                                    Phoenix.update (Incoming SocketClosed) initModel
                            in
                            newModel.socket.hasClosed
                                |> Expect.true "Expected the socket to be closed"
                    , test "sets socket isConnected to false" <|
                        \_ ->
                            let
                                connectedModel =
                                    { initModel | socket = { initSocket | isConnected = True } }

                                ( model, _ ) =
                                    Phoenix.update (Incoming SocketClosed) connectedModel
                            in
                            model.socket.isConnected
                                |> Expect.false "Expected the socket not to be connected"
                    ]
                , describe "SocketErrored"
                    [ test "sets socket hasErrored" <|
                        \_ ->
                            let
                                payload =
                                    { topic = "", message = "", payload = Encode.object [] }

                                ( model, _ ) =
                                    initModel
                                        |> Phoenix.update (Incoming <| SocketErrored payload)
                            in
                            model.socket.hasErrored
                                |> Expect.true "Expected the socket to have errored"
                    , test "sets socket isConnected to false" <|
                        \_ ->
                            let
                                payload =
                                    { topic = "", message = "", payload = Encode.object [] }

                                connectedModel =
                                    { initModel | socket = { initSocket | isConnected = True } }

                                ( model, _ ) =
                                    Phoenix.update (Incoming <| SocketErrored payload) connectedModel
                            in
                            model.socket.isConnected
                                |> Expect.false "Expected the socket not to be connected"
                    ]
                , describe "SocketOpened"
                    [ test "sets isConnected on the socket" <|
                        \_ ->
                            let
                                ( model, _ ) =
                                    Phoenix.update (Incoming SocketOpened) initModel
                            in
                            model.socket.isConnected
                                |> Expect.true "Expected socket to be connected"
                    ]
                ]
            , describe "Outgoing" <|
                let
                    initSocket =
                        Socket.init "/socket"

                    initModel =
                        Phoenix.initialize initSocket fakeSend
                in
                [ describe "createSocket"
                    [ test "returns the socket as is" <|
                        \_ ->
                            let
                                command =
                                    Phoenix.Message.createSocket initSocket

                                ( model, _ ) =
                                    Phoenix.update command initModel
                            in
                            Expect.equal model.socket initSocket
                    ]
                , describe "disconnect"
                    [ test "returns the socket as is" <|
                        \_ ->
                            let
                                cmd =
                                    Phoenix.Message.disconnect

                                ( model, _ ) =
                                    Phoenix.update cmd initModel
                            in
                            Expect.equal model initModel
                    ]
                , describe "createChannel"
                    [ test "puts the channel in the socket's channels dictionary" <|
                        \_ ->
                            let
                                channel =
                                    Channel.init "room:lobby"

                                cmd =
                                    Phoenix.Message.createChannel channel

                                ( model, _ ) =
                                    Phoenix.update cmd initModel
                            in
                            Expect.equal (Dict.get "room:lobby" model.channels) (Just channel)
                    ]
                , describe "leaveChannel"
                    [ test "has no immediate effect on the socket" <|
                        \_ ->
                            let
                                channel =
                                    Channel.init "room:lobby"

                                modelWithChannel =
                                    { initModel | channels = Dict.insert channel.topic channel initSocket.channels }

                                cmd =
                                    Phoenix.Message.leaveChannel channel

                                ( model, _ ) =
                                    Phoenix.update cmd modelWithChannel
                            in
                            Expect.equal model modelWithChannel
                    ]
                , describe "createPush"
                    [ test "adds a push to the socket's pushes dictionary" <|
                        \_ ->
                            let
                                push =
                                    Push.init "room:lobby" "my_event"

                                cmd =
                                    Phoenix.Message.createPush push

                                ( model, _ ) =
                                    Phoenix.update cmd initModel
                            in
                            Expect.equal (Dict.get push.topic model.pushes) (Just push)
                    ]
                ]
            ]
        ]


type Msg
    = FakeSendMsg Phoenix.Message.Data


fakeSend : Phoenix.Message.Data -> Cmd Msg
fakeSend =
    Task.perform identity << Task.succeed << FakeSendMsg
