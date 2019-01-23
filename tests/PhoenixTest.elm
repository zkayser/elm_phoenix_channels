module PhoenixTest exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode as Decode
import Json.Encode as Encode
import Phoenix
import Phoenix.Message exposing (Event(..), Message(..), PhoenixCommand(..))
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
            ]
        ]
