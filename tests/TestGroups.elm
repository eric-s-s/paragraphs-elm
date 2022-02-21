module TestGroups exposing (..)

import Expect
import Groups exposing (..)
import Test exposing (..)
import Word exposing (Infinitive(..), IrregularPast(..), Noun(..), NounBase(..), Particle(..), Preposition(..), Pronoun(..), Verb(..), Word(..))
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
