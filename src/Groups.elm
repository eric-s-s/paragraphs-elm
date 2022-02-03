module Groups exposing (..)

import Word exposing (Noun, Pronoun, Word(..), toObject, toSubject)
import WordData exposing (NumberOfObjects(..), VerbData, getBasicVerb)


type Subject
    = NounSubject Noun
    | PronounSubject Pronoun


subjectToWord : Subject -> Word
subjectToWord nounLike =
    case nounLike of
        NounSubject noun ->
            noun |> Noun

        PronounSubject pronoun ->
            pronoun |> toSubject |> Pronoun


type Object
    = NounObject Noun
    | PronounObject Pronoun


objectToWord : Object -> Word
objectToWord nounLike =
    case nounLike of
        NounObject noun ->
            noun |> Noun

        PronounObject pronoun ->
            pronoun |> toObject |> Pronoun


type Predicate
    = Predicate (List Word)


toPredicate : ( Object, Object ) -> VerbData -> Predicate
toPredicate ( firstObject, secondObject ) verbData =
    let
        verb =
            getBasicVerb verbData |> Verb
    in
    case verbData.numberOfObjects of
        One ->
            case ( verbData.particle, verbData.preposition, firstObject ) of
                ( Nothing, Just prep, _ ) ->
                    [ verb, prep |> Preposition, firstObject |> objectToWord ]
                        |> Predicate

                ( Just particle, Nothing, NounObject _ ) ->
                    [ verb, particle |> Particle, firstObject |> objectToWord ]
                        |> Predicate

                ( Just particle, Nothing, PronounObject _ ) ->
                    [ verb, firstObject |> objectToWord, particle |> Particle ]
                        |> Predicate

                ( Just particle, Just prep, _ ) ->
                    [ verb, particle |> Particle, prep |> Preposition, firstObject |> objectToWord ]
                        |> Predicate

                _ ->
                    [ verb, firstObject |> objectToWord ] |> Predicate

        Two ->
            case ( verbData.particle, verbData.preposition, firstObject ) of
                ( Just particle, Just prep, PronounObject _ ) ->
                    [ verb
                    , firstObject |> objectToWord
                    , particle |> Particle
                    , prep |> Preposition
                    , secondObject |> objectToWord
                    ]
                        |> Predicate

                ( Just particle, Just prep, NounObject _ ) ->
                    [ verb
                    , particle |> Particle
                    , firstObject |> objectToWord
                    , prep |> Preposition
                    , secondObject |> objectToWord
                    ]
                        |> Predicate

                ( Nothing, Just prep, _ ) ->
                    [ verb, firstObject |> objectToWord, prep |> Preposition, secondObject |> objectToWord ]
                        |> Predicate

                _ ->
                    [ verb, firstObject |> objectToWord, secondObject |> objectToWord ]
                        |> Predicate
