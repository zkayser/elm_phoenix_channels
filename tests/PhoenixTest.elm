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
import Test exposing (..)


suite : Test
suite =
    describe "Phoenix"
        [ describe "update"
            [ describe "Incoming"
                [ describe "SocketClosed"
                    [ test "sets socket state to closed" <|
                        \_ ->
                            let
                                ( newSocket, _ ) =
                                    Socket.init "/socket"
                                        |> Phoenix.update (Incoming SocketClosed)
                            in
                            newSocket.hasClosed
                                |> Expect.true "Expected the socket to be closed"
                    , test "sets socket isConnected to false" <|
                        \_ ->
                            let
                                socket =
                                    Socket.init "/socket"

                                connectedSocket =
                                    { socket | isConnected = True }

                                ( newSocket, _ ) =
                                    Phoenix.update (Incoming SocketClosed) connectedSocket
                            in
                            newSocket.isConnected
                                |> Expect.false "Expected the socket not to be connected"
                    ]
                , describe "SocketErrored"
                    [ test "sets socket hasErrored" <|
                        \_ ->
                            let
                                payload =
                                    { topic = "", message = "", payload = Encode.object [] }

                                ( newSocket, _ ) =
                                    Socket.init "/socket"
                                        |> Phoenix.update (Incoming <| SocketErrored payload)
                            in
                            newSocket.hasErrored
                                |> Expect.true "Expected the socket to have errored"
                    , test "sets socket isConnected to false" <|
                        \_ ->
                            let
                                payload =
                                    { topic = "", message = "", payload = Encode.object [] }

                                socket =
                                    Socket.init "/socket"

                                connectedSocket =
                                    { socket | isConnected = True }

                                ( newSocket, _ ) =
                                    Phoenix.update (Incoming <| SocketErrored payload) connectedSocket
                            in
                            newSocket.isConnected
                                |> Expect.false "Expected the socket not to be connected"
                    ]
                , describe "SocketOpened"
                    [ test "sets isConnected on the socket" <|
                        \_ ->
                            let
                                ( newSocket, _ ) =
                                    Socket.init "/socket"
                                        |> Phoenix.update (Incoming SocketOpened)
                            in
                            newSocket.isConnected
                                |> Expect.true "Expected socket to be connected"
                    ]
                ]
            , describe "Outgoing" <|
                let
                    initSocket = Socket.init "/socket"
                in
                [ describe "createSocket"
                    [ test "returns the socket as is" <|
                        \_ ->
                            let
                                command =
                                    Phoenix.Message.createSocket initSocket
                                ( socket, _ ) = Phoenix.update command initSocket
                            in
                            Expect.equal socket initSocket
                    ]
                , describe "disconnect"
                    [ test "returns the socket as is" <|
                        \_ ->
                            let
                                cmd = Phoenix.Message.disconnect
                                ( socket, _ ) = Phoenix.update cmd initSocket
                            in
                            Expect.equal socket initSocket
                    ]
                , describe "createChannel"
                    [ test "puts the channel in the socket's channels dictionary" <|
                        \_ ->
                            let
                                channel =
                                    Channel.init "room:lobby"

                                cmd = Phoenix.Message.createChannel channel

                                ( socket, _ ) =
                                    Phoenix.update cmd initSocket
                            in
                            Expect.equal (Dict.get "room:lobby" socket.channels) (Just channel)
                    ]
                , describe "leaveChannel"
                    [ test "has no immediate effect on the socket" <|
                        \_ ->
                            let
                                channel =
                                    Channel.init "room:lobby"

                                socketWithChannel =
                                    { initSocket | channels = Dict.insert channel.topic channel initSocket.channels }

                                cmd =
                                    Phoenix.Message.leaveChannel channel
                                ( socket, _ ) = Phoenix.update cmd socketWithChannel
                            in
                            Expect.equal socket initSocket
                    ]
                , describe "createPush"
                    [ test "adds a push to the socket's pushes dictionary" <|
                        \_ ->
                            let
                                push =
                                    Push.init "room:lobby" "my_event"

                                cmd =
                                    Phoenix.Message.createPush push

                                ( socket, _ ) =
                                    Phoenix.update cmd initSocket
                            in
                            Expect.equal (Dict.get push.topic socket.pushes) (Just push)
                    ]
                ]
            ]
        ]
