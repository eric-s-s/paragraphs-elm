module TestWord exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

import Word


suite : Test
suite =
    test "some tags" (\_ -> Expect.equal Word.Definite  Word.Plural)
