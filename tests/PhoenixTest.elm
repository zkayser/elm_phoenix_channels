module PhoenixTest exposing (suite)

import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode as Decode
import Json.Encode as Encode
import Phoenix
import Phoenix.Channel as Channel exposing (Channel)
import Phoenix.Message exposing (Event(..), Message(..), PhoenixCommand(..))
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
            , describe "Outgoing"
                [ describe "createSocket"
                    [ test "returns the socket as is" <|
                        \_ ->
                            let
                                ( socket, _ ) =
                                    Phoenix.Message.createSocket (Socket.init "/socket")
                            in
                            Expect.equal socket (Socket.init "/socket")
                    , test "returns a CreateSocket PhoenixCommand" <|
                        \_ ->
                            let
                                ( _, cmd ) =
                                    Phoenix.Message.createSocket (Socket.init "/socket")
                            in
                            Expect.equal cmd (CreateSocket (Socket.init "/socket"))
                    ]
                , describe "disconnect"
                    [ test "returns the socket as is" <|
                        \_ ->
                            let
                                ( socket, _ ) =
                                    Phoenix.Message.disconnect (Socket.init "/socket")
                            in
                            Expect.equal socket (Socket.init "/socket")
                    , test "returns a Disconnect PhoenixCommand" <|
                        \_ ->
                            let
                                ( _, cmd ) =
                                    Phoenix.Message.disconnect (Socket.init "/socket")
                            in
                            Expect.equal cmd Disconnect
                    ]
                , describe "createChannel"
                    [ test "puts the channel in the socket's channels dictionary" <|
                        \_ ->
                            let
                                channel =
                                    Channel.init "room:lobby"

                                ( socket, _ ) =
                                    Phoenix.Message.createChannel channel (Socket.init "/socket")
                            in
                            Expect.equal (Dict.get "room:lobby" socket.channels) (Just channel)
                    , test "returns a CreateChannel command with the passed in channel" <|
                        \_ ->
                            let
                                channel =
                                    Channel.init "room:lobby"

                                ( _, cmd ) =
                                    Phoenix.Message.createChannel channel (Socket.init "/socket")
                            in
                            Expect.equal cmd (CreateChannel channel)
                    ]
                , describe "leaveChannel"
                    [ test "removes the channel from the socket's channels dictionary" <|
                        \_ ->
                            let
                                socket =
                                    Socket.init "/socket"

                                channel =
                                    Channel.init "room:lobby"

                                socketWithChannel =
                                    { socket | channels = Dict.insert channel.topic channel socket.channels }

                                ( updatedSocket, _ ) =
                                    Phoenix.Message.leaveChannel channel socket
                            in
                            Expect.equal (Dict.get channel.topic updatedSocket.channels) Nothing
                    , test "returns a LeaveChannel message with the passed in channel" <|
                        \_ ->
                            let
                                channel =
                                    Channel.init "room:lobby"

                                ( _, cmd ) =
                                    Phoenix.Message.leaveChannel channel (Socket.init "/socket")
                            in
                            Expect.equal cmd (LeaveChannel channel)
                    ]
                , describe "createPush"
                    [ test "adds a push to the socket's pushes dictionary" <|
                        \_ ->
                            let
                                push =
                                    Push.init "room:lobby" "my_event"

                                ( socket, _ ) =
                                    Phoenix.Message.createPush push (Socket.init "/socket")
                            in
                            Expect.equal (Dict.get push.topic socket.pushes) (Just push)
                    , test "returns a CreatePush command with the passed in push" <|
                        \_ ->
                            let
                                push =
                                    Push.init "room:lobby" "my_event"

                                ( _, cmd ) =
                                    Phoenix.Message.createPush push (Socket.init "/socket")
                            in
                            Expect.equal cmd (CreatePush push)
                    ]
                ]
            ]
        ]
