module TestGroups exposing (..)

import Expect
import Groups exposing (..)
import Test exposing (..)
import Word exposing (BaseNoun(..), Infinitive(..), IrregularPast(..), Noun(..), ParticleType(..), PrepositionType(..), Pronoun(..), Verb(..), Word(..), stringToPreposition)
import WordData exposing (NumberOfObjects(..), VerbData)


toNounObject : String -> Object
toNounObject inputString =
    ProperNoun (BaseNoun inputString) |> NounObject


testPredicate : Test
testPredicate =
    describe "from verb data"
        [ test "one no preposition no particle object nouns" <|
            \_ ->
                VerbData "see" NoIrregularPast One Nothing Nothing
                    |> toPredicate ( toNounObject "dog", toNounObject "cat" )
                    |> Expect.equal
                        ([ BasicVerb (Infinitive "see") NoIrregularPast |> Verb
                         , ProperNoun (BaseNoun "dog") |> Noun
                         ]
                            |> Predicate
                        )
        , test "one preposition no particle object" <|
            \_ ->
                VerbData "run" NoIrregularPast One Nothing (PrepositionType "over" |> Just)
                    |> toPredicate ( I |> PronounObject, He |> PronounObject )
                    |> Expect.equal
                        ([ BasicVerb (Infinitive "run") NoIrregularPast |> Verb
                         , "over" |> PrepositionType |> Preposition
                         , Me |> Pronoun
                         ]
                            |> Predicate
                        )
        , test "one no preposition particle noun object" <|
            \_ ->
                VerbData "pick" NoIrregularPast One (SeparableParticle "up" |> Just) Nothing
                    |> toPredicate ( toNounObject "dog", He |> PronounObject )
                    |> Expect.equal
                        ([ BasicVerb (Infinitive "pick") NoIrregularPast |> Verb
                         , "up" |> SeparableParticle |> Particle
                         , ProperNoun (BaseNoun "dog") |> Noun
                         ]
                            |> Predicate
                        )
        , test "one no preposition particle pronnoun object" <|
            \_ ->
                VerbData "pick" NoIrregularPast One (SeparableParticle "up" |> Just) Nothing
                    |> toPredicate ( I |> PronounObject, He |> PronounObject )
                    |> Expect.equal
                        ([ BasicVerb (Infinitive "pick") NoIrregularPast |> Verb
                         , Me |> Pronoun
                         , "up" |> SeparableParticle |> Particle
                         ]
                            |> Predicate
                        )
        , test "one  preposition particle one object" <|
            \_ ->
                VerbData "clean" NoIrregularPast One (SeparableParticle "up" |> Just) (PrepositionType "with" |> Just)
                    |> toPredicate ( I |> PronounObject, He |> PronounObject )
                    |> Expect.equal
                        ([ BasicVerb (Infinitive "clean") NoIrregularPast |> Verb
                         , "up" |> SeparableParticle |> Particle
                         , "with" |> PrepositionType |> Preposition
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
                         , ProperNoun (BaseNoun "dog") |> Noun
                         , ProperNoun (BaseNoun "cat") |> Noun
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
                VerbData "clean" NoIrregularPast Two (SeparableParticle "up" |> Just) (PrepositionType "with" |> Just)
                    |> toPredicate ( I |> PronounObject, He |> PronounObject )
                    |> Expect.equal
                        ([ BasicVerb (Infinitive "clean") NoIrregularPast |> Verb
                         , Me |> Pronoun
                         , "up" |> SeparableParticle |> Particle
                         , "with" |> PrepositionType |> Preposition
                         , Him |> Pronoun
                         ]
                            |> Predicate
                        )
        , test "two, preposition, particle two object nouns" <|
            \_ ->
                VerbData "clean" NoIrregularPast Two (SeparableParticle "up" |> Just) (PrepositionType "with" |> Just)
                    |> toPredicate ( toNounObject "dog", toNounObject "cat" )
                    |> Expect.equal
                        ([ BasicVerb (Infinitive "clean") NoIrregularPast |> Verb
                         , "up" |> SeparableParticle |> Particle
                         , ProperNoun (BaseNoun "dog") |> Noun
                         , "with" |> PrepositionType |> Preposition
                         , ProperNoun (BaseNoun "cat") |> Noun
                         ]
                            |> Predicate
                        )
        , test "two, preposition, no particle any object" <|
            \_ ->
                VerbData "clean" NoIrregularPast Two Nothing (PrepositionType "with" |> Just)
                    |> toPredicate ( He |> PronounObject, toNounObject "cat" )
                    |> Expect.equal
                        ([ BasicVerb (Infinitive "clean") NoIrregularPast |> Verb
                         , Him |> Pronoun
                         , "with" |> PrepositionType |> Preposition
                         , ProperNoun (BaseNoun "cat") |> Noun
                         ]
                            |> Predicate
                        )
        ]
