module Phoenix.ChannelTest exposing (suite)

import Expect
import Fuzz
import Test exposing (..)


suite : Test
suite =
    describe "some stuff"
        [ test "that it does" <|
            \_ ->
                Expect.equal True True
        ]
