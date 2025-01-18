module TestTextGenerator exposing (..)

import Expect
import List exposing (map)
import String exposing (join, split)
import Test exposing (..)
import Paragraphs exposing (..)
import TextGenerator exposing (..)

testGenerateText : Test
testGenerateText =
    test "the right text" <|
        \_ ->
            generateText
                |> Expect.equal "some text"

