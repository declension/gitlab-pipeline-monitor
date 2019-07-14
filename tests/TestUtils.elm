module TestUtils exposing (suite)

import Expect exposing (Expectation)
import Test exposing (..)
import Utils exposing (stripQuestion)


suite : Test
suite =
    describe "The TestUtils module"
        [ describe "stripQuestion"
            [ test "takes one question mark off" <|
                \_ ->
                    "?foo=bar"
                        |> stripQuestion
                        |> Expect.equal "foo=bar"
            , test "leaves a normal string alone" <|
                \_ ->
                    "ABCDEFG"
                        |> stripQuestion
                        |> Expect.equal "ABCDEFG"
            ]
        ]
