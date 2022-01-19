module TestWord exposing (..)

import Expect
import Test exposing (..)

import Word exposing (Pronoun(..))
import Word exposing (toValue)


pronounToStringTest : Test
pronounToStringTest = 
    describe "all pronoun string conversion"
        [ test "base conversion" <|
            (\_ ->
                let
                    pronouns = 
                        [I, Me, You, He, Him, She, Her, We, Us, They, Them]
                in
                pronouns
                    |> List.map toValue
                    |> Expect.equal [
                        "I",
                        "Me",
                        "You",
                        "He",
                        "Him",
                        "She",
                        "Her",
                        "We",
                        "Us",
                        "They",
                        "Them"
                    ]

            )

        ]