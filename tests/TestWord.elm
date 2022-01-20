module TestWord exposing (..)

import Expect
import Test exposing (..)

import Words.Word exposing (..)
import List exposing (map)
import String exposing (split)
import String exposing (join)


pronounToStringTest : Test
pronounToStringTest = 
    describe "all pronoun string conversion as words"
        [ test "toString uppercase" <|

            (\_ ->
                let
                    pronouns = 
                        [I, Me, You, He, Him, She, Her, It, We, Us, They, Them]
                in
                pronouns
                    |> List.map (\el -> Pronoun el Capital)
                    |> List.map wordToString
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
                    |> List.map wordToString
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

nounToStringTest: Test
nounToStringTest = 
    describe "testing nounToString"
        [   test "base noun to value" <|
                (\_ ->
                    BaseNoun (BaseValue "dog") (IrregularPlural Nothing) 
                    |> nounToString 
                    |> Expect.equal "dog"
                )
            , test "indefinite noun to value as a" <|
                (\_ ->
                    ["dog", "cat", "mouse"]
                    |> map (\el -> IndefiniteNoun (BaseValue el) (IrregularPlural Nothing))
                    |> map nounToString
                    |> Expect.equal ["a dog", "a cat", "a mouse"]              
                )
            , test "indefinite noun to value as an" <|
                (\_ ->
                    split "" "aeiouAEIOU"
                    |> map (\el -> IndefiniteNoun (BaseValue el) (IrregularPlural Nothing))
                    |> map nounToString
                    |> join ", "
                    |> Expect.equal "an a, an e, an i, an o, an u, an A, an E, an I, an O, an U"            
                )
            , test "definite noun to value" <|
                (\_ ->
                    DefiniteNoun (BaseValue "dog") (IrregularPlural Nothing) 
                    |> nounToString 
                    |> Expect.equal "the dog"
                )
            , test "plural noun to value irregular" <|
                (\_ ->
                    PluralNoun (BaseValue "dog") (IrregularPlural (Just "doggies")) 
                    |> nounToString 
                    |> Expect.equal "doggies"
                )
            , test "plural noun to value s ending" <|
                (\_ ->
                    PluralNoun (BaseValue "dog") (IrregularPlural Nothing) 
                    |> nounToString 
                    |> Expect.equal "dogs"
                )
            , test "plural noun to value es ending" <|
                (\_ ->
                    ["mass", "bobo", "ex", "watch", "dish"]
                    |> map (\el -> PluralNoun (BaseValue el) (IrregularPlural Nothing)) 
                    |> map nounToString 
                    |> Expect.equal ["masses", "boboes", "exes", "watches", "dishes"]
                )
            , test "plural noun to value y as vowel" <|
                (\_ ->
                    ["day", "boy", "caddy", "baby", "key"]
                    |> map (\el -> PluralNoun (BaseValue el) (IrregularPlural Nothing)) 
                    |> map nounToString 
                    |> Expect.equal ["days", "boys", "caddies", "babies", "keys"]
                )
            , test "plural noun to value f endings" <|
                (\_ ->
                    ["life", "waif", "calf", "leaf", "wolf", "wharf"]
                    |> map (\el -> PluralNoun (BaseValue el) (IrregularPlural Nothing)) 
                    |> map nounToString 
                    |> Expect.equal ["lives", "waifs", "calves", "leaves", "wolves", "wharves"]
                )
            , test "definite plural noun to value irregular" <|
                (\_ ->
                    DefinitePluralNoun (BaseValue "dog") (IrregularPlural (Just "doggies")) 
                    |> nounToString 
                    |> Expect.equal "the doggies"
                )
            , test "definite plural noun to value" <|
                (\_ ->
                    ["life", "waif", "baby", "day", "bobo", "ex", "dog"]
                    |> map (\el -> DefinitePluralNoun (BaseValue el) (IrregularPlural Nothing)) 
                    |> map nounToString 
                    |> Expect.equal ["the lives", "the waifs", "the babies","the days", "the boboes", "the exes", "the dogs"]
                )
            , test "uncountable noun to value" <|
                (\_ ->
                    UncountableNoun (BaseValue "water") 
                    |> nounToString 
                    |> Expect.equal "water"
                )
            , test "definite uncountable noun to value" <|
                (\_ ->
                    DefiniteUncountableNoun (BaseValue "water") 
                    |> nounToString 
                    |> Expect.equal "the water"
                )
            , test "proper noun to value" <|
                (\_ ->
                    ProperNoun (BaseValue "Eric") 
                    |> nounToString 
                    |> Expect.equal "Eric"
                )
            , test "proper plural noun to value" <|
                (\_ ->
                    ProperPluralNoun (BaseValue "the Bobs") 
                    |> nounToString 
                    |> Expect.equal "the Bobs"
                )
            , test "incorrect noun to value" <|
                (\_ ->
                    ProperPluralNoun (BaseValue "the Bobs")
                    |> IncorrectNoun "the the Bobs"
                    |> nounToString 
                    |> Expect.equal "the the Bobs"
                )
        ]

baseValue = BaseValue "nonsense"
plural = IrregularPlural <| Just "nonsensicals"

testToDefinite: Test
testToDefinite = 
    describe "convert nouns to definite"
        [ test "base, indefinite, definite noun" <|
            \_ ->
                [BaseNoun, IndefiniteNoun, DefiniteNoun]
                |> map (\el -> el baseValue plural)
                |> map toDefinite
                |> Expect.equal (List.repeat 3 (DefiniteNoun baseValue plural))
        , test "plurals" <|
            \_ ->
                [PluralNoun, DefinitePluralNoun]
                |> map (\el -> el baseValue plural)
                |> map toDefinite
                |> Expect.equal (List.repeat 2 (DefinitePluralNoun baseValue plural))
        , test "uncountables" <|
            \_ ->
                [UncountableNoun, DefiniteUncountableNoun]
                |> map (\el -> el baseValue)
                |> map toDefinite
                |> Expect.equal (List.repeat 2 (DefiniteUncountableNoun baseValue ))
        , test "propers" <|
            (\_ ->
                let
                    originals = ([ProperNoun, ProperPluralNoun] |> map (\el -> el baseValue))
                in
                originals
                |> map (\el-> IncorrectNoun ("the " ++ nounToString el) el)
                |> Expect.equal (map toDefinite originals)
            )
        , test "incorrect noun" <|
            (\_ ->
                let
                    inner = baseValue |> \(BaseValue el) -> el 
                    noun = ProperNoun baseValue
                in
                IncorrectNoun ("the " ++ inner) noun
                |> toDefinite
                |> Expect.equal (IncorrectNoun ("the the " ++ inner) noun)
            )
        ]