module TestWord exposing (..)

import Expect
import List exposing (map)
import String exposing (join, split)
import Test exposing (..)
import Word exposing (..)


testHelperRawValueToString : RawValue -> String
testHelperRawValueToString (RawValue string) =
    string


testHelperNounToString : Noun -> String
testHelperNounToString =
    nounToRawValue >> testHelperRawValueToString


baseValue : BaseNoun
baseValue =
    BaseNoun "nonsense"


plural : IrregularPlural
plural =
    IrregularPlural "nonsensicals"


testNoun : Test
testNoun =
    describe "test Noun"
        [ describe "nounToRawValue"
            [ test "base noun to value" <|
                \_ ->
                    BasicNoun (BaseNoun "dog") NoIrregularPlural
                        |> testHelperNounToString
                        |> Expect.equal "dog"
            , test "indefinite noun to value as a" <|
                \_ ->
                    [ "dog", "cat", "mouse" ]
                        |> map (\el -> IndefiniteNoun (BaseNoun el) NoIrregularPlural)
                        |> map testHelperNounToString
                        |> Expect.equal [ "a dog", "a cat", "a mouse" ]
            , test "indefinite noun to value as an" <|
                \_ ->
                    split "" "aeiouAEIOU"
                        |> map (\el -> IndefiniteNoun (BaseNoun el) NoIrregularPlural)
                        |> map testHelperNounToString
                        |> join ", "
                        |> Expect.equal
                            "an a, an e, an i, an o, an u, an A, an E, an I, an O, an U"
            , test "definite noun to value" <|
                \_ ->
                    DefiniteNoun (BaseNoun "dog") NoIrregularPlural
                        |> testHelperNounToString
                        |> Expect.equal "the dog"
            , test "plural noun to value irregular" <|
                \_ ->
                    PluralNoun (BaseNoun "dog") (IrregularPlural "doggies")
                        |> testHelperNounToString
                        |> Expect.equal "doggies"
            , test "plural noun to value s ending" <|
                \_ ->
                    PluralNoun (BaseNoun "dog") NoIrregularPlural
                        |> testHelperNounToString
                        |> Expect.equal "dogs"
            , test "plural noun to value es ending" <|
                \_ ->
                    [ "mass", "bobo", "ex", "watch", "dish" ]
                        |> map (\el -> PluralNoun (BaseNoun el) NoIrregularPlural)
                        |> map testHelperNounToString
                        |> Expect.equal [ "masses", "boboes", "exes", "watches", "dishes" ]
            , test "plural noun to value y as vowel" <|
                \_ ->
                    [ "day", "boy", "caddy", "baby", "key" ]
                        |> map (\el -> PluralNoun (BaseNoun el) NoIrregularPlural)
                        |> map testHelperNounToString
                        |> Expect.equal [ "days", "boys", "caddies", "babies", "keys" ]
            , test "plural noun to value f endings" <|
                \_ ->
                    [ "life", "waif", "calf", "leaf", "wolf", "wharf" ]
                        |> map (\el -> PluralNoun (BaseNoun el) NoIrregularPlural)
                        |> map testHelperNounToString
                        |> Expect.equal
                            [ "lives", "waifs", "calves", "leaves", "wolves", "wharves" ]
            , test "definite plural noun to value irregular" <|
                \_ ->
                    DefinitePluralNoun (BaseNoun "dog") (IrregularPlural "doggies")
                        |> testHelperNounToString
                        |> Expect.equal "the doggies"
            , test "definite plural noun to value" <|
                \_ ->
                    [ "life", "waif", "baby", "day", "bobo", "ex", "dog" ]
                        |> map (\el -> DefinitePluralNoun (BaseNoun el) NoIrregularPlural)
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
            ]
        , describe "test toOriginalNoun"
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
        , describe "toIndefinite"
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
                                |> map (\el -> PluralNoun (BaseNoun el) NoIrregularPlural)
                    in
                    nouns
                        |> map toIndefinite
                        |> Expect.equal (List.map2 (\a b -> IncorrectNoun a b) [ "a dogs", "an apes" ] nouns)
            , test "definite plural" <|
                \_ ->
                    let
                        nouns =
                            [ "cat", "owl" ]
                                |> map (\el -> DefinitePluralNoun (BaseNoun el) NoIrregularPlural)
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
        , describe "toDefinite"
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
        , describe "toPlural"
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
        ]


infinitive : Infinitive
infinitive =
    Infinitive "somerandomaction"


testVerb : Test
testVerb =
    describe "testing Verb"
        [ describe "verbToRawValue"
            [ test "IncorrectVerb" <|
                \_ ->
                    BasicVerb (Infinitive "eat") NoIrregularPast
                        |> IncorrectVerb "ooooops"
                        |> verbToRawValue
                        |> Expect.equal (RawValue "ooooops")
            , test "BasicVerb" <|
                \_ ->
                    BasicVerb (Infinitive "eat") NoIrregularPast
                        |> verbToRawValue
                        |> Expect.equal (RawValue "eat")
            , test "NegativeVerb" <|
                \_ ->
                    Negative (Infinitive "eat") NoIrregularPast
                        |> verbToRawValue
                        |> Expect.equal (RawValue "don't eat")
            , describe "ThirdPerson"
                [ test "ThirdPerson add s" <|
                    \_ ->
                        [ "eat", "play", "know", "see" ]
                            |> map (\el -> ThirdPerson (Infinitive el) NoIrregularPast)
                            |> map verbToRawValue
                            |> map testHelperRawValueToString
                            |> Expect.equal [ "eats", "plays", "knows", "sees" ]
                , test "ThirdPerson add es" <|
                    \_ ->
                        [ "fix", "go", "pass", "watch", "wash", "fuzz" ]
                            |> map (\el -> ThirdPerson (Infinitive el) NoIrregularPast)
                            |> map verbToRawValue
                            |> map testHelperRawValueToString
                            |> Expect.equal [ "fixes", "goes", "passes", "watches", "washes", "fuzzes" ]
                , test "ThirdPerson add ies" <|
                    \_ ->
                        [ "try", "baby" ]
                            |> map (\el -> ThirdPerson (Infinitive el) NoIrregularPast)
                            |> map verbToRawValue
                            |> map testHelperRawValueToString
                            |> Expect.equal [ "tries", "babies" ]
                , test "ThirdPerson have has" <|
                    \_ ->
                        ThirdPerson (Infinitive "have") NoIrregularPast
                            |> verbToRawValue
                            |> Expect.equal (RawValue "has")
                ]
            , test "ThirdPersonNegative" <|
                \_ ->
                    ThirdPersonNegative (Infinitive "abc") NoIrregularPast
                        |> verbToRawValue
                        |> Expect.equal (RawValue "doesn't abc")
            , describe "Past"
                [ test "two consonants ed" <|
                    \_ ->
                        [ "hack", "pant", "sort", "vest" ]
                            |> map (\el -> Past (Infinitive el) NoIrregularPast)
                            |> map verbToRawValue
                            |> map testHelperRawValueToString
                            |> Expect.equal [ "hacked", "panted", "sorted", "vested" ]
                , test "compound vowels ed" <|
                    \_ ->
                        [ "shoo", "toy", "vow", "play", "key" ]
                            |> map (\el -> Past (Infinitive el) NoIrregularPast)
                            |> map verbToRawValue
                            |> map testHelperRawValueToString
                            |> Expect.equal [ "shooed", "toyed", "vowed", "played", "keyed" ]
                , test "long vowel consonant ed" <|
                    \_ ->
                        [ "shout", "plead", "seem" ]
                            |> map (\el -> Past (Infinitive el) NoIrregularPast)
                            |> map verbToRawValue
                            |> map testHelperRawValueToString
                            |> Expect.equal [ "shouted", "pleaded", "seemed" ]
                , test "y as long vowel ied" <|
                    \_ ->
                        [ "try", "bandy" ]
                            |> map (\el -> Past (Infinitive el) NoIrregularPast)
                            |> map verbToRawValue
                            |> map testHelperRawValueToString
                            |> Expect.equal [ "tried", "bandied" ]
                , test "double consonant ed" <|
                    \_ ->
                        [ "pat", "strip", "regret", "cop", "cup", "ab" ]
                            |> map (\el -> Past (Infinitive el) NoIrregularPast)
                            |> map verbToRawValue
                            |> map testHelperRawValueToString
                            |> Expect.equal [ "patted", "stripped", "regretted", "copped", "cupped", "abbed" ]
                , test "verb ending in 'e'" <|
                    \_ ->
                        [ "like", "pee", "shoe" ]
                            |> map (\el -> Past (Infinitive el) NoIrregularPast)
                            |> map verbToRawValue
                            |> map testHelperRawValueToString
                            |> Expect.equal [ "liked", "peed", "shoed" ]
                , test "edge cases" <|
                    \_ ->
                        [ "", "a", "b" ]
                            |> map (\el -> Past (Infinitive el) NoIrregularPast)
                            |> map verbToRawValue
                            |> map testHelperRawValueToString
                            |> Expect.equal [ "ed", "aed", "bed" ]
                , test "irregular verb" <|
                    \_ ->
                        Past (Infinitive "abc") (IrregularPast "went")
                            |> verbToRawValue
                            |> testHelperRawValueToString
                            |> Expect.equal "went"
                ]
            , test "PastNegative" <|
                \_ ->
                    PastNegative (Infinitive "abc") (IrregularPast "def")
                        |> verbToRawValue
                        |> testHelperRawValueToString
                        |> Expect.equal "didn't abc"
            ]
        , describe "toOriginalVerb"
            [ test "BasicVerb to BasicVerb" <|
                \_ ->
                    BasicVerb (Infinitive "abc") (IrregularPast "def")
                        |> toOriginalVerb
                        |> Expect.equal (BasicVerb (Infinitive "abc") (IrregularPast "def"))
            , test "Negative, ThirdPerson, ThirdPersonNegative, Past, PastNegative to BasicVerb" <|
                \_ ->
                    [ Negative, ThirdPerson, ThirdPersonNegative, Past, PastNegative ]
                        |> map (\el -> el infinitive NoIrregularPast)
                        |> map toOriginalVerb
                        |> Expect.equal (List.repeat 5 (BasicVerb infinitive NoIrregularPast))
            , test "IncorrectVerb to BasicVerb" <|
                \_ ->
                    [ Negative, ThirdPerson, ThirdPersonNegative, Past, PastNegative ]
                        |> map (\el -> IncorrectVerb "ooops" (el infinitive NoIrregularPast))
                        |> map toOriginalVerb
                        |> Expect.equal (List.repeat 5 (BasicVerb infinitive NoIrregularPast))
            ]
        , describe "toThirdPerson"
            [ test "BasicVerb to ThirdPerson" <|
                \_ ->
                    BasicVerb infinitive NoIrregularPast
                        |> toThirdPerson
                        |> Expect.equal (ThirdPerson infinitive NoIrregularPast)
            , test "Negative to ThirdPersonNegative" <|
                \_ ->
                    Negative infinitive NoIrregularPast
                        |> toThirdPerson
                        |> Expect.equal (ThirdPersonNegative infinitive NoIrregularPast)
            , test "Past to IncorrectVerb regular past" <|
                \_ ->
                    let
                        original =
                            Past (Infinitive "play") NoIrregularPast
                    in
                    original
                        |> toThirdPerson
                        |> Expect.equal (IncorrectVerb "playeds" original)
            , test "Past to IncorrectVerb irregular past" <|
                \_ ->
                    let
                        original =
                            Past infinitive (IrregularPast "went")
                    in
                    original
                        |> toThirdPerson
                        |> Expect.equal (IncorrectVerb "wents" original)
            , test "PastNegative to IncorrectVerb" <|
                \_ ->
                    let
                        original =
                            PastNegative (Infinitive "play") NoIrregularPast
                    in
                    original
                        |> toThirdPerson
                        |> Expect.equal (IncorrectVerb "didn't plays" original)
            , test "any ThirdPerson to Itself" <|
                \_ ->
                    let
                        original =
                            [ ThirdPerson infinitive NoIrregularPast
                            , ThirdPersonNegative infinitive NoIrregularPast
                            ]
                    in
                    original
                        |> map toThirdPerson
                        |> Expect.equal original
            ]
        , describe "toNegative"
            [ test "BasicVerb to Negative" <|
                \_ ->
                    BasicVerb infinitive NoIrregularPast
                        |> toNegative
                        |> Expect.equal (Negative infinitive NoIrregularPast)
            , test "ThirdPerson to ThirdPersonNegative" <|
                \_ ->
                    ThirdPerson infinitive NoIrregularPast
                        |> toNegative
                        |> Expect.equal (ThirdPersonNegative infinitive NoIrregularPast)
            , test "Past to PastNegative" <|
                \_ ->
                    Past infinitive NoIrregularPast
                        |> toNegative
                        |> Expect.equal (PastNegative infinitive NoIrregularPast)
            , test "any Negative to Itself" <|
                \_ ->
                    let
                        original =
                            [ Negative infinitive NoIrregularPast
                            , ThirdPersonNegative infinitive NoIrregularPast
                            , PastNegative infinitive NoIrregularPast
                            ]
                    in
                    original
                        |> map toNegative
                        |> Expect.equal original
            , describe "toPast"
                [ test "BasicVerb to Past" <|
                    \_ ->
                        BasicVerb infinitive NoIrregularPast
                            |> toPast
                            |> Expect.equal (Past infinitive NoIrregularPast)
                , test "Negative to PastNegative" <|
                    \_ ->
                        Negative infinitive NoIrregularPast
                            |> toPast
                            |> Expect.equal (PastNegative infinitive NoIrregularPast)
                , test "ThirdPerson to Incorrect no irregular" <|
                    \_ ->
                        let
                            original =
                                ThirdPerson (Infinitive "play") NoIrregularPast
                        in
                        original
                            |> toPast
                            |> Expect.equal (IncorrectVerb "playeds" original)
                , test "ThirdPerson to Incorrect irregular" <|
                    \_ ->
                        let
                            original =
                                ThirdPerson infinitive (IrregularPast "went")
                        in
                        original
                            |> toPast
                            |> Expect.equal (IncorrectVerb "wents" original)
                , test "ThirdPersonNegative to Incorrect no irregular" <|
                    \_ ->
                        let
                            original =
                                ThirdPersonNegative (Infinitive "play") NoIrregularPast
                        in
                        original
                            |> toPast
                            |> Expect.equal (IncorrectVerb "doesn't played" original)
                , test "ThirdPersonNegative to Incorrect irregular" <|
                    \_ ->
                        let
                            original =
                                ThirdPersonNegative infinitive (IrregularPast "went")
                        in
                        original
                            |> toPast
                            |> Expect.equal (IncorrectVerb "doesn't went" original)
                , test "Past to Past" <|
                    \_ ->
                        Past infinitive NoIrregularPast
                            |> toPast
                            |> Expect.equal (Past infinitive NoIrregularPast)
                , test "PastNegative to PastNegative" <|
                    \_ ->
                        PastNegative infinitive NoIrregularPast
                            |> toPast
                            |> Expect.equal (PastNegative infinitive NoIrregularPast)
                ]
            ]
        ]


testPronoun : Test
testPronoun =
    describe "test Pronou"
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
        , describe "test toSubbject"
            [ test "unchanged" <|
                \_ ->
                    [ I, You, He, She, It, We, They ]
                        |> map toSubject
                        |> Expect.equal [ I, You, He, She, It, We, They ]
            , test "changed" <|
                \_ ->
                    [ Me, Him, Her, Us, Them ]
                        |> map toSubject
                        |> Expect.equal [ I, He, She, We, They ]
            ]
        , describe "test toObject"
            [ test "unchanged" <|
                \_ ->
                    [ Me, You, Him, Her, It, Us, Them ]
                        |> map toObject
                        |> Expect.equal [ Me, You, Him, Her, It, Us, Them ]
            , test "changed" <|
                \_ ->
                    [ I, He, She, We, They ]
                        |> map toObject
                        |> Expect.equal [ Me, Him, Her, Us, Them ]
            ]
        ]


testPunctuation : Test
testPunctuation =
    test "punctuationToRawValue" <|
        \_ ->
            [ Period, ExclamationPoint, QuestionMark, Comma ]
                |> map punctuationToRawValue
                |> map testHelperRawValueToString
                |> Expect.equal [ ".", "!", "?", "," ]


testBeVerb : Test
testBeVerb =
    describe "BeVerb tests"
        [ test "beVerbToRawValue" <|
            \_ ->
                [ Is, Am, Are, Was, Were ]
                    |> map beVerbToRawValue
                    |> map testHelperRawValueToString
                    |> Expect.equal [ "is", "am", "are", "was", "were" ]
        , test "beVerbToPast" <|
            \_ ->
                [ Is, Am, Are, Was, Were ]
                    |> map beVerbToPast
                    |> Expect.equal [ Was, Was, Were, Was, Were ]
        ]


testWord : Test
testWord =
    describe "toRawValue"
        [ test "Noun" <|
            \_ ->
                [ BasicNoun (BaseNoun "cat") NoIrregularPlural
                , IndefiniteNoun (BaseNoun "ax") NoIrregularPlural
                , DefiniteNoun (BaseNoun "thing") NoIrregularPlural
                , PluralNoun (BaseNoun "dog") NoIrregularPlural
                , DefinitePluralNoun baseValue (IrregularPlural "thingen")
                , UncountableNoun (BaseNoun "air")
                , DefiniteUncountableNoun (BaseNoun "water")
                , ProperNoun (BaseNoun "Eric")
                , ProperPluralNoun (BaseNoun "the Joes")
                , IncorrectNoun "oops" (ProperNoun baseValue)
                ]
                    |> map Noun
                    |> map wordToValue
                    |> Expect.equal
                        (map RawValue
                            [ "cat"
                            , "an ax"
                            , "the thing"
                            , "dogs"
                            , "the thingen"
                            , "air"
                            , "the water"
                            , "Eric"
                            , "the Joes"
                            , "oops"
                            ]
                        )
        , test "Verb" <|
            \_ ->
                [ BasicVerb (Infinitive "go") NoIrregularPast
                , ThirdPerson (Infinitive "pass") NoIrregularPast
                , Negative (Infinitive "eat") NoIrregularPast
                , ThirdPersonNegative (Infinitive "fly") NoIrregularPast
                , Past (Infinitive "pat") NoIrregularPast
                , Past infinitive (IrregularPast "went")
                , PastNegative (Infinitive "do") NoIrregularPast
                , IncorrectVerb "ooops" (Negative infinitive NoIrregularPast)
                ]
                    |> map Verb
                    |> map wordToValue
                    |> Expect.equal
                        (map RawValue
                            [ "go"
                            , "passes"
                            , "don't eat"
                            , "doesn't fly"
                            , "patted"
                            , "went"
                            , "didn't do"
                            , "ooops"
                            ]
                        )
        , test "Puncutation" <|
            \_ ->
                [ Period, ExclamationPoint, QuestionMark, Comma ]
                    |> map Punctuation
                    |> map wordToValue
                    |> Expect.equal
                        (map RawValue [ ".", "!", "?", "," ])
        , test "Pronoun" <|
            \_ ->
                [ I, Me, You, It, He, Him, She, Her, We, Us, They, Them ]
                    |> map Pronoun
                    |> map wordToValue
                    |> Expect.equal
                        (map RawValue
                            [ "I"
                            , "me"
                            , "you"
                            , "it"
                            , "he"
                            , "him"
                            , "she"
                            , "her"
                            , "we"
                            , "us"
                            , "they"
                            , "them"
                            ]
                        )
        , test "BeVerb" <|
            \_ ->
                [ Is, Am, Are, Was, Were ]
                    |> map BeVerb
                    |> map wordToValue
                    |> Expect.equal (map RawValue [ "is", "am", "are", "was", "were" ])
        , test "NegativeBeVerb" <|
            \_ ->
                [ Is, Am, Are, Was, Were ]
                    |> map NegativeBeVerb
                    |> map wordToValue
                    |> Expect.equal
                        (map
                            RawValue
                            [ "is not", "am not", "are not", "was not", "were not" ]
                        )
        , test "Preposition" <|
            \_ -> Preposition (PrepositionType "abc") |> wordToValue |> Expect.equal (RawValue "abc")
        , test "SeparableParticle" <|
            \_ -> Particle (SeparableParticle "def") |> wordToValue |> Expect.equal (RawValue "def")
        ]
