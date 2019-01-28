module SocketTest exposing (suite)

import Expect
import Phoenix.Socket as Socket exposing (Socket)
import Test exposing (..)


suite : Test
suite =
    describe "Socket" <|
        let
            socket =
                Socket.init "/socket"
        in
        [ describe "close"
            [ test "sets hasClosed and isConnected fields" <|
                \_ ->
                    let
                        updatedSocket =
                            Socket.close socket
                    in
                    Expect.all
                        [ \s -> Expect.true "Expected socket to be closed" s.hasClosed
                        , \s -> Expect.false "Expected socket not to be connected" s.isConnected
                        ]
                        updatedSocket
            ]
        , describe "errored"
            [ test "sets hasErrored and isConnected fields" <|
                \_ ->
                    let
                        updatedSocket =
                            Socket.errored socket
                    in
                    Expect.all
                        [ \s -> Expect.true "Expected socket to have errored" s.hasErrored
                        , \s -> Expect.false "Expected socket not to be connected" s.isConnected
                        ]
                        updatedSocket
            ]
        , describe "opened"
            [ test "sets isConnected field" <|
                \_ ->
                    let
                        notConnected =
                            { socket | isConnected = False }

                        updatedSocket =
                            Socket.opened notConnected
                    in
                    Expect.true "Expected socket to be connected" updatedSocket.isConnected
            ]
        ]
