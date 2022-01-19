module TestWord exposing (..)

import Expect
import Test exposing (..)

import Word exposing (BasePronoun(..))
import Word exposing (toValue)
import Word exposing (Word(..))
import Word exposing (Capitalized(..))


pronounToStringTest : Test
pronounToStringTest = 
    describe "all pronoun string conversion"
        [ test "base conversion" <|
            (\_ ->
                let
                    pronouns = 
                        [I, Me, You, He, Him, She, Her, It, We, Us, They, Them]
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
                        "It",
                        "We",
                        "Us",
                        "They",
                        "Them"
                    ]

            ),
        test "toString uppercase" <|

            (\_ ->
                let
                    pronouns = 
                        [I, Me, You, He, Him, She, Her, It, We, Us, They, Them]
                in
                pronouns
                    |> List.map (\el -> Pronoun el Capital)
                    |> List.map Word.toString
                    |> Expect.equal [
                        "I",
                        "Me",
                        "You",
                        "He",
                        "Him",
                        "She",
                        "Her",
                        "It",
                        "We",
                        "Us",
                        "They",
                        "Them"
                    ]

            ),
        test "toString lowercase" <|

            (\_ ->
                let
                    pronouns = 
                        [I, Me, You, He, Him, She, Her,It, We, Us, They, Them]
                in
                pronouns
                    |> List.map (\el -> Pronoun el Lowercase)
                    |> List.map Word.toString
                    |> Expect.equal [
                        "I",
                        "me",
                        "you",
                        "he",
                        "him",
                        "she",
                        "her",
                        "it",
                        "we",
                        "us",
                        "they",
                        "them"
                    ]

            )

        ]