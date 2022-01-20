module TestWord exposing (..)

import Expect
import Test exposing (..)

import Words.Word exposing (..)
import List exposing (map)
import String exposing (split)
import String exposing (join)


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
                    |> List.map toString
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
                    |> List.map toString
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

nounTest: Test
nounTest = 
    describe "testing nouns"
        [   test "base noun to value" <|
                (\_ ->
                    BaseNoun (BaseValue "dog") (IrregularPlural Nothing) 
                    |> toNounValue 
                    |> Expect.equal "dog"
                )
            , test "indefinite noun to value as a" <|
                (\_ ->
                    ["dog", "cat", "mouse"]
                    |> map (\el -> IndefiniteNoun (BaseValue el) (IrregularPlural Nothing))
                    |> map toNounValue
                    |> Expect.equal ["a dog", "a cat", "a mouse"]              
                )
            , test "indefinite noun to value as an" <|
                (\_ ->
                    split "" "aeiouAEIOU"
                    |> map (\el -> IndefiniteNoun (BaseValue el) (IrregularPlural Nothing))
                    |> map toNounValue
                    |> join ", "
                    |> Expect.equal "an a, an e, an i, an o, an u, an A, an E, an I, an O, an U"            
                )
            , test "definite noun to value" <|
                (\_ ->
                    DefiniteNoun (BaseValue "dog") (IrregularPlural Nothing) 
                    |> toNounValue 
                    |> Expect.equal "the dog"
                )
            , test "plural noun to value irregular" <|
                (\_ ->
                    PluralNoun (BaseValue "dog") (IrregularPlural (Just "doggies")) 
                    |> toNounValue 
                    |> Expect.equal "doggies"
                )
            , test "plural noun to value s ending" <|
                (\_ ->
                    PluralNoun (BaseValue "dog") (IrregularPlural Nothing) 
                    |> toNounValue 
                    |> Expect.equal "dogs"
                )
            , test "plural noun to value es ending" <|
                (\_ ->
                    ["mass", "bobo", "ex", "watch", "dish"]
                    |> map (\el -> PluralNoun (BaseValue el) (IrregularPlural Nothing)) 
                    |> map toNounValue 
                    |> Expect.equal ["masses", "boboes", "exes", "watches", "dishes"]
                )
            , test "plural noun to value y as vowel" <|
                (\_ ->
                    ["day", "boy", "caddy", "baby", "key"]
                    |> map (\el -> PluralNoun (BaseValue el) (IrregularPlural Nothing)) 
                    |> map toNounValue 
                    |> Expect.equal ["days", "boys", "caddies", "babies", "keys"]
                )
            , test "plural noun to value f endings" <|
                (\_ ->
                    ["life", "waif", "calf", "leaf", "wolf", "wharf"]
                    |> map (\el -> PluralNoun (BaseValue el) (IrregularPlural Nothing)) 
                    |> map toNounValue 
                    |> Expect.equal ["lives", "waifs", "calves", "leaves", "wolves", "wharves"]
                )
            , test "definite plural noun to value irregular" <|
                (\_ ->
                    DefinitePluralNoun (BaseValue "dog") (IrregularPlural (Just "doggies")) 
                    |> toNounValue 
                    |> Expect.equal "the doggies"
                )
            , test "definite plural noun to value" <|
                (\_ ->
                    ["life", "waif", "baby", "day", "bobo", "ex", "dog"]
                    |> map (\el -> DefinitePluralNoun (BaseValue el) (IrregularPlural Nothing)) 
                    |> map toNounValue 
                    |> Expect.equal ["the lives", "the waifs", "the babies","the days", "the boboes", "the exes", "the dogs"]
                )
            , test "uncountable noun to value" <|
                (\_ ->
                    UncountableNoun (BaseValue "water") 
                    |> toNounValue 
                    |> Expect.equal "water"
                )
            , test "definite uncountable noun to value" <|
                (\_ ->
                    DefiniteUncountableNoun (BaseValue "water") 
                    |> toNounValue 
                    |> Expect.equal "the water"
                )
            , test "proper noun to value" <|
                (\_ ->
                    ProperNoun (BaseValue "Eric") 
                    |> toNounValue 
                    |> Expect.equal "Eric"
                )
            , test "proper plural noun to value" <|
                (\_ ->
                    ProperPluralNoun (BaseValue "the Bobs") 
                    |> toNounValue 
                    |> Expect.equal "the Bobs"
                )
            , test "incorrect noun to value" <|
                (\_ ->
                    ProperPluralNoun (BaseValue "the Bobs")
                    |> IncorrectNoun "the the Bobs"
                    |> toNounValue 
                    |> Expect.equal "the the Bobs"
                )
        ]