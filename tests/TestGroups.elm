module TestGroups exposing (..)

import Expect
import Groups exposing (..)
import List exposing (map)
import Test exposing (..)
import Word
    exposing
        ( Infinitive(..)
        , IrregularPast(..)
        , IrregularPlural(..)
        , Noun(..)
        , NounBase(..)
        , Particle(..)
        , Preposition(..)
        , Pronoun(..)
        , Punctuation(..)
        , Verb(..)
        , Word(..)
        )
import WordData exposing (NumberOfObjects(..), VerbData)


toNounObject : String -> Object
toNounObject inputString =
    ProperNoun (NounBase inputString) |> NounObject


testPredicate : Test
testPredicate =
    describe "from verb data"
        [ test "one no preposition no particle object nouns" <|
            \_ ->
                VerbData "see" NoIrregularPast One Nothing Nothing
                    |> toPredicate ( toNounObject "dog", toNounObject "cat" )
                    |> Expect.equal
                        ([ BasicVerb (Infinitive "see") NoIrregularPast |> Verb
                         , ProperNoun (NounBase "dog") |> Noun
                         ]
                            |> Predicate
                        )
        , test "one preposition no particle object" <|
            \_ ->
                VerbData "play" NoIrregularPast One Nothing (SimplePreposition "with" |> Just)
                    |> toPredicate ( I |> PronounObject, He |> PronounObject )
                    |> Expect.equal
                        ([ BasicVerb (Infinitive "play") NoIrregularPast |> Verb
                         , "with" |> SimplePreposition |> Preposition
                         , Me |> Pronoun
                         ]
                            |> Predicate
                        )
        , test "one no preposition particle noun object" <|
            \_ ->
                VerbData "pick" NoIrregularPast One (AdverbialParticle "up" |> Just) Nothing
                    |> toPredicate ( toNounObject "dog", He |> PronounObject )
                    |> Expect.equal
                        ([ BasicVerb (Infinitive "pick") NoIrregularPast |> Verb
                         , "up" |> AdverbialParticle |> Particle
                         , ProperNoun (NounBase "dog") |> Noun
                         ]
                            |> Predicate
                        )
        , test "one no preposition particle pronnoun object" <|
            \_ ->
                VerbData "pick" NoIrregularPast One (AdverbialParticle "up" |> Just) Nothing
                    |> toPredicate ( I |> PronounObject, He |> PronounObject )
                    |> Expect.equal
                        ([ BasicVerb (Infinitive "pick") NoIrregularPast |> Verb
                         , Me |> Pronoun
                         , "up" |> AdverbialParticle |> Particle
                         ]
                            |> Predicate
                        )
        , test "one  preposition particle one object" <|
            \_ ->
                VerbData "clean" NoIrregularPast One (AdverbialParticle "up" |> Just) (SimplePreposition "with" |> Just)
                    |> toPredicate ( I |> PronounObject, He |> PronounObject )
                    |> Expect.equal
                        ([ BasicVerb (Infinitive "clean") NoIrregularPast |> Verb
                         , "up" |> AdverbialParticle |> Particle
                         , "with" |> SimplePreposition |> Preposition
                         , Me |> Pronoun
                         ]
                            |> Predicate
                        )
        , test "two, no preposition, no particle two object nouns" <|
            \_ ->
                VerbData "give" NoIrregularPast Two Nothing Nothing
                    |> toPredicate ( toNounObject "dog", toNounObject "cat" )
                    |> Expect.equal
                        ([ BasicVerb (Infinitive "give") NoIrregularPast |> Verb
                         , ProperNoun (NounBase "dog") |> Noun
                         , ProperNoun (NounBase "cat") |> Noun
                         ]
                            |> Predicate
                        )
        , test "two, no preposition, no particle, two object pronouns" <|
            \_ ->
                VerbData "give" NoIrregularPast Two Nothing Nothing
                    |> toPredicate ( I |> PronounObject, He |> PronounObject )
                    |> Expect.equal
                        ([ BasicVerb (Infinitive "give") NoIrregularPast |> Verb
                         , Me |> Pronoun
                         , Him |> Pronoun
                         ]
                            |> Predicate
                        )
        , test "two, preposition, particle two object pronouns" <|
            \_ ->
                VerbData "clean" NoIrregularPast Two (AdverbialParticle "up" |> Just) (SimplePreposition "with" |> Just)
                    |> toPredicate ( I |> PronounObject, He |> PronounObject )
                    |> Expect.equal
                        ([ BasicVerb (Infinitive "clean") NoIrregularPast |> Verb
                         , Me |> Pronoun
                         , "up" |> AdverbialParticle |> Particle
                         , "with" |> SimplePreposition |> Preposition
                         , Him |> Pronoun
                         ]
                            |> Predicate
                        )
        , test "two, preposition, particle two object nouns" <|
            \_ ->
                VerbData "clean" NoIrregularPast Two (AdverbialParticle "up" |> Just) (SimplePreposition "with" |> Just)
                    |> toPredicate ( toNounObject "dog", toNounObject "cat" )
                    |> Expect.equal
                        ([ BasicVerb (Infinitive "clean") NoIrregularPast |> Verb
                         , "up" |> AdverbialParticle |> Particle
                         , ProperNoun (NounBase "dog") |> Noun
                         , "with" |> SimplePreposition |> Preposition
                         , ProperNoun (NounBase "cat") |> Noun
                         ]
                            |> Predicate
                        )
        , test "two, preposition, no particle any object" <|
            \_ ->
                VerbData "clean" NoIrregularPast Two Nothing (SimplePreposition "with" |> Just)
                    |> toPredicate ( He |> PronounObject, toNounObject "cat" )
                    |> Expect.equal
                        ([ BasicVerb (Infinitive "clean") NoIrregularPast |> Verb
                         , Him |> Pronoun
                         , "with" |> SimplePreposition |> Preposition
                         , ProperNoun (NounBase "cat") |> Noun
                         ]
                            |> Predicate
                        )
        ]


testingPredicate : Predicate
testingPredicate =
    VerbData "eat" (IrregularPast "ate") One (AdverbialParticle "up" |> Just) Nothing
        |> toPredicate ( He |> PronounObject, toNounObject "notused" )


testingSimplePresent : Subject -> Sentence
testingSimplePresent subject =
    SimplePresent subject testingPredicate Period


testingNegativeSimplePresent : Subject -> Sentence
testingNegativeSimplePresent subject =
    NegativeSimplePresent subject testingPredicate Period


testingSimplePast : Subject -> Sentence
testingSimplePast subject =
    SimplePast subject testingPredicate Period


testingNegativeSimplePast : Subject -> Sentence
testingNegativeSimplePast subject =
    NegativeSimplePast subject testingPredicate Period


testSentence : Test
testSentence =
    describe "sentenceToString"
        [ describe "SimplePresent"
            [ test "third person pronoun" <|
                \_ ->
                    [ He, Him, Her, She, It ]
                        |> map PronounSubject
                        |> map testingSimplePresent
                        |> map sentenceToString
                        |> Expect.equal
                            [ "He eats him up."
                            , "He eats him up."
                            , "She eats him up."
                            , "She eats him up."
                            , "It eats him up."
                            ]
            , test "third person noun" <|
                \_ ->
                    [ IndefiniteNoun (NounBase "dog") NoIrregularPlural
                    , DefiniteNoun (NounBase "cat") NoIrregularPlural
                    , ProperNoun (NounBase "the Chad")
                    , ProperNoun (NounBase "Joe")
                    , UncountableNoun (NounBase "water")
                    , DefiniteUncountableNoun (NounBase "air")
                    ]
                        |> map NounSubject
                        |> map testingSimplePresent
                        |> map sentenceToString
                        |> Expect.equal
                            [ "A dog eats him up."
                            , "The cat eats him up."
                            , "The Chad eats him up."
                            , "Joe eats him up."
                            , "Water eats him up."
                            , "The air eats him up."
                            ]
            , test "non third person pronouns" <|
                \_ ->
                    [ I, Me, You, We, Us, They, Them ]
                        |> map PronounSubject
                        |> map testingSimplePresent
                        |> map sentenceToString
                        |> Expect.equal
                            [ "I eat him up."
                            , "I eat him up."
                            , "You eat him up."
                            , "We eat him up."
                            , "We eat him up."
                            , "They eat him up."
                            , "They eat him up."
                            ]
            , test "non third person noun" <|
                \_ ->
                    [ PluralNoun (NounBase "child") (IrregularPlural "children")
                    , DefinitePluralNoun (NounBase "cat") NoIrregularPlural
                    , ProperPluralNoun (NounBase "the Joneses")
                    , ProperPluralNoun (NounBase "BMWs")
                    ]
                        |> map NounSubject
                        |> map testingSimplePresent
                        |> map sentenceToString
                        |> Expect.equal
                            [ "Children eat him up."
                            , "The cats eat him up."
                            , "The Joneses eat him up."
                            , "BMWs eat him up."
                            ]
            ]
        , describe "NegativeSimplePresent"
            [ test "third person pronoun" <|
                \_ ->
                    [ He, Him, Her, She, It ]
                        |> map PronounSubject
                        |> map testingNegativeSimplePresent
                        |> map sentenceToString
                        |> Expect.equal
                            [ "He doesn't eat him up."
                            , "He doesn't eat him up."
                            , "She doesn't eat him up."
                            , "She doesn't eat him up."
                            , "It doesn't eat him up."
                            ]
            , test "third person noun" <|
                \_ ->
                    [ IndefiniteNoun (NounBase "dog") NoIrregularPlural
                    , DefiniteNoun (NounBase "cat") NoIrregularPlural
                    , ProperNoun (NounBase "the Chad")
                    , ProperNoun (NounBase "Joe")
                    , UncountableNoun (NounBase "water")
                    , DefiniteUncountableNoun (NounBase "air")
                    ]
                        |> map NounSubject
                        |> map testingNegativeSimplePresent
                        |> map sentenceToString
                        |> Expect.equal
                            [ "A dog doesn't eat him up."
                            , "The cat doesn't eat him up."
                            , "The Chad doesn't eat him up."
                            , "Joe doesn't eat him up."
                            , "Water doesn't eat him up."
                            , "The air doesn't eat him up."
                            ]
            , test "non third person pronouns" <|
                \_ ->
                    [ I, Me, You, We, Us, They, Them ]
                        |> map PronounSubject
                        |> map testingNegativeSimplePresent
                        |> map sentenceToString
                        |> Expect.equal
                            [ "I don't eat him up."
                            , "I don't eat him up."
                            , "You don't eat him up."
                            , "We don't eat him up."
                            , "We don't eat him up."
                            , "They don't eat him up."
                            , "They don't eat him up."
                            ]
            , test "non third person noun" <|
                \_ ->
                    [ PluralNoun (NounBase "child") (IrregularPlural "children")
                    , DefinitePluralNoun (NounBase "cat") NoIrregularPlural
                    , ProperPluralNoun (NounBase "the Joneses")
                    , ProperPluralNoun (NounBase "BMWs")
                    ]
                        |> map NounSubject
                        |> map testingNegativeSimplePresent
                        |> map sentenceToString
                        |> Expect.equal
                            [ "Children don't eat him up."
                            , "The cats don't eat him up."
                            , "The Joneses don't eat him up."
                            , "BMWs don't eat him up."
                            ]
            ]
        , describe "SimplePast"
            [ test "pronoun" <|
                \_ ->
                    [ He, Him, She, It, I, Me, You, We, They ]
                        |> map PronounSubject
                        |> map testingSimplePast
                        |> map sentenceToString
                        |> Expect.equal
                            [ "He ate him up."
                            , "He ate him up."
                            , "She ate him up."
                            , "It ate him up."
                            , "I ate him up."
                            , "I ate him up."
                            , "You ate him up."
                            , "We ate him up."
                            , "They ate him up."
                            ]
            , test "noun" <|
                \_ ->
                    [ IndefiniteNoun (NounBase "dog") NoIrregularPlural
                    , DefiniteNoun (NounBase "cat") NoIrregularPlural
                    , ProperNoun (NounBase "the Chad")
                    , UncountableNoun (NounBase "water")
                    , DefiniteUncountableNoun (NounBase "air")
                    , DefinitePluralNoun (NounBase "cat") NoIrregularPlural
                    ]
                        |> map NounSubject
                        |> map testingSimplePast
                        |> map sentenceToString
                        |> Expect.equal
                            [ "A dog ate him up."
                            , "The cat ate him up."
                            , "The Chad ate him up."
                            , "Water ate him up."
                            , "The air ate him up."
                            , "The cats ate him up."
                            ]
            ]
        , describe "NegativeSimplePast"
            [ test "pronoun" <|
                \_ ->
                    [ He, Him, She, It, I, Me, You, We, They ]
                        |> map PronounSubject
                        |> map testingNegativeSimplePast
                        |> map sentenceToString
                        |> Expect.equal
                            [ "He didn't eat him up."
                            , "He didn't eat him up."
                            , "She didn't eat him up."
                            , "It didn't eat him up."
                            , "I didn't eat him up."
                            , "I didn't eat him up."
                            , "You didn't eat him up."
                            , "We didn't eat him up."
                            , "They didn't eat him up."
                            ]
            , test "noun" <|
                \_ ->
                    [ IndefiniteNoun (NounBase "dog") NoIrregularPlural
                    , DefiniteNoun (NounBase "cat") NoIrregularPlural
                    , ProperNoun (NounBase "the Chad")
                    , UncountableNoun (NounBase "water")
                    , DefiniteUncountableNoun (NounBase "air")
                    , DefinitePluralNoun (NounBase "cat") NoIrregularPlural
                    ]
                        |> map NounSubject
                        |> map testingNegativeSimplePast
                        |> map sentenceToString
                        |> Expect.equal
                            [ "A dog didn't eat him up."
                            , "The cat didn't eat him up."
                            , "The Chad didn't eat him up."
                            , "Water didn't eat him up."
                            , "The air didn't eat him up."
                            , "The cats didn't eat him up."
                            ]
            ]
        ]
