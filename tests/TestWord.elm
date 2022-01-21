module TestWord exposing (..)

import Expect
import List exposing (map)
import String exposing (join, split)
import Test exposing (..)
import Words.Word exposing (..)


testHelperRawValueToString : RawValue -> String
testHelperRawValueToString (RawValue string) =
    string


testPronounToRawValue : Test
testPronounToRawValue =
    describe "pronouns to raw values"
        [ test "to raw values" <|
            \_ ->
                [ I, Me, You, He, Him, She, Her, It, We, Us, They, Them ]
                    |> List.map pronounToRawValue
                    |> List.map testHelperRawValueToString
                    |> Expect.equal
                        [ "I"
                        , "me"
                        , "you"
                        , "he"
                        , "him"
                        , "she"
                        , "her"
                        , "it"
                        , "we"
                        , "us"
                        , "they"
                        , "them"
                        ]
        , test "as word" <|
            \_ ->
                Pronoun I |> wordToValue |> Expect.equal (RawValue "I")
        ]


testHelperNounToString : Noun -> String
testHelperNounToString =
    nounToRawValue >> testHelperRawValueToString


testNounToRawValue : Test
testNounToRawValue =
    describe "testing nounToRawValue"
        [ test "base noun to value" <|
            \_ ->
                BasicNoun (BaseNoun "dog") (IrregularPlural Nothing)
                    |> testHelperNounToString
                    |> Expect.equal "dog"
        , test "indefinite noun to value as a" <|
            \_ ->
                [ "dog", "cat", "mouse" ]
                    |> map (\el -> IndefiniteNoun (BaseNoun el) (IrregularPlural Nothing))
                    |> map testHelperNounToString
                    |> Expect.equal [ "a dog", "a cat", "a mouse" ]
        , test "indefinite noun to value as an" <|
            \_ ->
                split "" "aeiouAEIOU"
                    |> map (\el -> IndefiniteNoun (BaseNoun el) (IrregularPlural Nothing))
                    |> map testHelperNounToString
                    |> join ", "
                    |> Expect.equal
                        "an a, an e, an i, an o, an u, an A, an E, an I, an O, an U"
        , test "definite noun to value" <|
            \_ ->
                DefiniteNoun (BaseNoun "dog") (IrregularPlural Nothing)
                    |> testHelperNounToString
                    |> Expect.equal "the dog"
        , test "plural noun to value irregular" <|
            \_ ->
                PluralNoun (BaseNoun "dog") (IrregularPlural (Just "doggies"))
                    |> testHelperNounToString
                    |> Expect.equal "doggies"
        , test "plural noun to value s ending" <|
            \_ ->
                PluralNoun (BaseNoun "dog") (IrregularPlural Nothing)
                    |> testHelperNounToString
                    |> Expect.equal "dogs"
        , test "plural noun to value es ending" <|
            \_ ->
                [ "mass", "bobo", "ex", "watch", "dish" ]
                    |> map (\el -> PluralNoun (BaseNoun el) (IrregularPlural Nothing))
                    |> map testHelperNounToString
                    |> Expect.equal [ "masses", "boboes", "exes", "watches", "dishes" ]
        , test "plural noun to value y as vowel" <|
            \_ ->
                [ "day", "boy", "caddy", "baby", "key" ]
                    |> map (\el -> PluralNoun (BaseNoun el) (IrregularPlural Nothing))
                    |> map testHelperNounToString
                    |> Expect.equal [ "days", "boys", "caddies", "babies", "keys" ]
        , test "plural noun to value f endings" <|
            \_ ->
                [ "life", "waif", "calf", "leaf", "wolf", "wharf" ]
                    |> map (\el -> PluralNoun (BaseNoun el) (IrregularPlural Nothing))
                    |> map testHelperNounToString
                    |> Expect.equal
                        [ "lives", "waifs", "calves", "leaves", "wolves", "wharves" ]
        , test "definite plural noun to value irregular" <|
            \_ ->
                DefinitePluralNoun (BaseNoun "dog") (IrregularPlural (Just "doggies"))
                    |> testHelperNounToString
                    |> Expect.equal "the doggies"
        , test "definite plural noun to value" <|
            \_ ->
                [ "life", "waif", "baby", "day", "bobo", "ex", "dog" ]
                    |> map (\el -> DefinitePluralNoun (BaseNoun el) (IrregularPlural Nothing))
                    |> map testHelperNounToString
                    |> Expect.equal
                        [ "the lives"
                        , "the waifs"
                        , "the babies"
                        , "the days"
                        , "the boboes"
                        , "the exes"
                        , "the dogs"
                        ]
        , test "uncountable noun to value" <|
            \_ ->
                UncountableNoun (BaseNoun "water")
                    |> testHelperNounToString
                    |> Expect.equal "water"
        , test "definite uncountable noun to value" <|
            \_ ->
                DefiniteUncountableNoun (BaseNoun "water")
                    |> testHelperNounToString
                    |> Expect.equal "the water"
        , test "proper noun to value" <|
            \_ ->
                ProperNoun (BaseNoun "Eric")
                    |> testHelperNounToString
                    |> Expect.equal "Eric"
        , test "proper plural noun to value" <|
            \_ ->
                ProperPluralNoun (BaseNoun "the Bobs")
                    |> testHelperNounToString
                    |> Expect.equal "the Bobs"
        , test "incorrect noun to value" <|
            \_ ->
                ProperPluralNoun (BaseNoun "the Bobs")
                    |> IncorrectNoun "the the Bobs"
                    |> testHelperNounToString
                    |> Expect.equal "the the Bobs"
        , test "as word" <|
            \_ ->
                [ ProperNoun (BaseNoun "Eric")
                , PluralNoun (BaseNoun "dog") (IrregularPlural Nothing)
                , DefiniteUncountableNoun (BaseNoun "water")
                ]
                    |> map Noun
                    |> map wordToValue
                    |> Expect.equal
                        [ RawValue "Eric"
                        , RawValue "dogs"
                        , RawValue "the water"
                        ]
        ]


baseValue : BaseNoun
baseValue =
    BaseNoun "nonsense"


plural : IrregularPlural
plural =
    IrregularPlural <| Just "nonsensicals"


testToOriginal : Test
testToOriginal =
    describe "test converting nouns to their most basic format" <|
        [ test "basic to basic" <|
            \_ ->
                BasicNoun baseValue plural
                    |> toOriginalNoun
                    |> Expect.equal (BasicNoun baseValue plural)
        , test "indefinite to basic" <|
            \_ ->
                IndefiniteNoun baseValue plural
                    |> toOriginalNoun
                    |> Expect.equal (BasicNoun baseValue plural)
        , test "definite to basic" <|
            \_ ->
                DefiniteNoun baseValue plural
                    |> toOriginalNoun
                    |> Expect.equal (BasicNoun baseValue plural)
        , test "plural to basic" <|
            \_ ->
                PluralNoun baseValue plural
                    |> toOriginalNoun
                    |> Expect.equal (BasicNoun baseValue plural)
        , test "definite plural to basic" <|
            \_ ->
                DefinitePluralNoun baseValue plural
                    |> toOriginalNoun
                    |> Expect.equal (BasicNoun baseValue plural)
        , test "uncountable to uncountable" <|
            \_ ->
                UncountableNoun baseValue
                    |> toOriginalNoun
                    |> Expect.equal (UncountableNoun baseValue)
        , test "definite uncountable to uncountable" <|
            \_ ->
                DefiniteUncountableNoun baseValue
                    |> toOriginalNoun
                    |> Expect.equal (UncountableNoun baseValue)
        , test "proper to proper" <|
            \_ ->
                ProperNoun baseValue
                    |> toOriginalNoun
                    |> Expect.equal (ProperNoun baseValue)
        , test "proper plural to proper plural" <|
            \_ ->
                ProperPluralNoun baseValue
                    |> toOriginalNoun
                    |> Expect.equal (ProperPluralNoun baseValue)
        , test "incorrect noun to container nouns basic form" <|
            \_ ->
                let
                    nouns =
                        [ BasicNoun baseValue plural
                        , IndefiniteNoun baseValue plural
                        , DefiniteNoun baseValue plural
                        , PluralNoun baseValue plural
                        , DefinitePluralNoun baseValue plural
                        , UncountableNoun baseValue
                        , DefiniteUncountableNoun baseValue
                        , ProperNoun baseValue
                        , ProperPluralNoun baseValue
                        ]
                in
                nouns
                    |> map (\el -> IncorrectNoun "something" el)
                    |> map toOriginalNoun
                    |> Expect.equal (nouns |> map toOriginalNoun)
        ]


testToIndefinite : Test
testToIndefinite =
    describe "convert nouns to indefinite"
        [ test "base, indefinite, definite noun" <|
            \_ ->
                [ BasicNoun, IndefiniteNoun, DefiniteNoun ]
                    |> map (\el -> el baseValue plural)
                    |> map toIndefinite
                    |> Expect.equal (List.repeat 3 (IndefiniteNoun baseValue plural))
        , test "plural" <|
            \_ ->
                let
                    nouns =
                        [ "dog", "ape" ]
                            |> map (\el -> PluralNoun (BaseNoun el) (IrregularPlural Nothing))
                in
                nouns
                    |> map toIndefinite
                    |> Expect.equal (List.map2 (\a b -> IncorrectNoun a b) [ "a dogs", "an apes" ] nouns)
        , test "definite plural" <|
            \_ ->
                let
                    nouns =
                        [ "cat", "owl" ]
                            |> map (\el -> DefinitePluralNoun (BaseNoun el) (IrregularPlural Nothing))
                in
                nouns
                    |> map toIndefinite
                    |> Expect.equal (List.map2 (\a b -> IncorrectNoun a b) [ "a cats", "an owls" ] nouns)
        , test "uncountable" <|
            \_ ->
                let
                    nouns =
                        [ UncountableNoun (BaseNoun "water")
                        , DefiniteUncountableNoun (BaseNoun "unstuff")
                        ]
                in
                nouns
                    |> map toIndefinite
                    |> Expect.equal
                        (List.map2
                            (\a b -> IncorrectNoun a b)
                            [ "a water", "an unstuff" ]
                            nouns
                        )
        , test "proper" <|
            \_ ->
                let
                    nouns =
                        [ ProperNoun (BaseNoun "Eric")
                        , ProperPluralNoun (BaseNoun "BMWs")
                        ]
                in
                nouns
                    |> map toIndefinite
                    |> Expect.equal
                        (List.map2
                            (\a b -> IncorrectNoun a b)
                            [ "an Eric", "a BMWs" ]
                            nouns
                        )
        ]


testToDefinite : Test
testToDefinite =
    describe "convert nouns to definite"
        [ test "base, indefinite, definite noun" <|
            \_ ->
                [ BasicNoun, IndefiniteNoun, DefiniteNoun ]
                    |> map (\el -> el baseValue plural)
                    |> map toDefinite
                    |> Expect.equal (List.repeat 3 (DefiniteNoun baseValue plural))
        , test "plurals" <|
            \_ ->
                [ PluralNoun, DefinitePluralNoun ]
                    |> map (\el -> el baseValue plural)
                    |> map toDefinite
                    |> Expect.equal (List.repeat 2 (DefinitePluralNoun baseValue plural))
        , test "uncountables" <|
            \_ ->
                [ UncountableNoun, DefiniteUncountableNoun ]
                    |> map (\el -> el baseValue)
                    |> map toDefinite
                    |> Expect.equal (List.repeat 2 (DefiniteUncountableNoun baseValue))
        , test "propers" <|
            \_ ->
                let
                    originals =
                        [ ProperNoun, ProperPluralNoun ] |> map (\el -> el baseValue)
                in
                originals
                    |> map (\el -> IncorrectNoun ("the " ++ testHelperNounToString el) el)
                    |> Expect.equal (map toDefinite originals)
        , test "incorrect noun" <|
            \_ ->
                let
                    inner =
                        baseValue |> (\(BaseNoun el) -> el)

                    noun =
                        ProperNoun baseValue
                in
                IncorrectNoun ("the " ++ inner) noun
                    |> toDefinite
                    |> Expect.equal (IncorrectNoun ("the the " ++ inner) noun)
        ]


testToPlural : Test
testToPlural =
    describe "test convert to plural nouns"
        [ test "plural, definite plural, proper plural unchanged" <|
            \_ ->
                let
                    nouns =
                        [ PluralNoun baseValue plural
                        , DefinitePluralNoun baseValue plural
                        , ProperPluralNoun baseValue
                        ]
                in
                nouns
                    |> map toPlural
                    |> Expect.equal nouns
        , test "basic to plural" <|
            \_ ->
                BasicNoun baseValue plural
                    |> toPlural
                    |> Expect.equal (PluralNoun baseValue plural)
        , test "definite to definite plural" <|
            \_ ->
                DefiniteNoun baseValue plural
                    |> toPlural
                    |> Expect.equal (DefinitePluralNoun baseValue plural)
        , test "proper to incorrect" <|
            \_ ->
                ProperNoun (BaseNoun "Jody")
                    |> toPlural
                    |> Expect.equal (IncorrectNoun "Jodies" (ProperNoun (BaseNoun "Jody")))
        , test "uncountable to incorrect" <|
            \_ ->
                UncountableNoun (BaseNoun "fizz")
                    |> toPlural
                    |> Expect.equal (IncorrectNoun "fizzes" (UncountableNoun (BaseNoun "fizz")))
        , test "definite uncountable to incorrect" <|
            \_ ->
                DefiniteUncountableNoun (BaseNoun "fizz")
                    |> toPlural
                    |> Expect.equal (IncorrectNoun "the fizzes" (DefiniteUncountableNoun (BaseNoun "fizz")))
        ]
