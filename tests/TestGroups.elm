module TestGroups exposing (..)

import Expect
import Test exposing (..)
import Groups exposing (..)
import WordData exposing (VerbData, NumberOfObjects(..))
import Word exposing (Noun(..))
import Word exposing (BaseNoun(..))
import Word exposing (Word(..))
import Word exposing (Verb(..))
import Word exposing (Infinitive(..))
import Word exposing (IrregularPast(..))
import Word exposing (Verb)


toNounObject:  String -> Object
toNounObject inputString = ProperNoun (BaseNoun inputString) |> NounObject

testPredicate: Test
testPredicate = 
    describe "from verb data"
        [ test "no preposition one object nouns" <|
            \_ -> 
                VerbData "see" Nothing One Nothing Nothing
                |> toPredicate (toNounObject "dog", toNounObject "cat")
                |> Expect.equal ([
                    (BasicVerb (Infinitive "see") NoIrregularPast) |> Verb
                    , (ProperNoun (BaseNoun "dog")) |> Noun
                ] |> Predicate)
        ]
