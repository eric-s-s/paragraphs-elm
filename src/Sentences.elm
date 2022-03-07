module Sentences exposing (..)

import Word
    exposing
        ( FormattedWord(..)
        , Noun(..)
        , Pronoun(..)
        , Punctuation
        , Verb
        , Word(..)
        , toNegative
        , toObject
        , toPast
        , toSubject
        , toThirdPerson
        , wordToString
        )
import WordData exposing (NumberOfObjects(..), VerbData, getBasicVerb)


type Subject
    = NounSubject Noun
    | PronounSubject Pronoun
    | WrongSubject Word


type Sentence
    = SimplePresent Subject Predicate Punctuation
    | NegativeSimplePresent Subject Predicate Punctuation
    | SimplePast Subject Predicate Punctuation
    | NegativeSimplePast Subject Predicate Punctuation


sentenceMap : (Word -> Word) -> Sentence -> Sentence
sentenceMap function sentence =
    case sentence of
        SimplePresent subject (Predicate words) punctuation ->
            SimplePresent
                (subject |> subjectToWord |> function |> wordToSubject)
                (words |> List.map function |> Predicate)
                punctuation

        NegativeSimplePresent subject (Predicate words) punctuation ->
            NegativeSimplePresent
                (subject |> subjectToWord |> function |> wordToSubject)
                (words |> List.map function |> Predicate)
                punctuation

        SimplePast subject (Predicate words) punctuation ->
            SimplePast
                (subject |> subjectToWord |> function |> wordToSubject)
                (words |> List.map function |> Predicate)
                punctuation

        NegativeSimplePast subject (Predicate words) punctuation ->
            NegativeSimplePast
                (subject |> subjectToWord |> function |> wordToSubject)
                (words |> List.map function |> Predicate)
                punctuation


sentenceToString : Sentence -> String
sentenceToString sentence =
    case sentence of
        SimplePresent subject predicate punctuation ->
            if isThirdPerson subject then
                sentencePartsToString subject (predicate |> transformVerb toThirdPerson) punctuation

            else
                sentencePartsToString subject predicate punctuation

        NegativeSimplePresent subject predicate punctuation ->
            if isThirdPerson subject then
                sentencePartsToString
                    subject
                    (predicate |> transformVerb (toThirdPerson >> toNegative))
                    punctuation

            else
                sentencePartsToString subject (predicate |> transformVerb toNegative) punctuation

        SimplePast subject predicate punctuation ->
            sentencePartsToString subject (predicate |> transformVerb toPast) punctuation

        NegativeSimplePast subject predicate punctuation ->
            sentencePartsToString subject (predicate |> transformVerb (toPast >> toNegative)) punctuation


sentencePartsToString : Subject -> Predicate -> Punctuation -> String
sentencePartsToString subject predicate punctuation =
    (subject |> subjectToWord |> Capital |> wordToString)
        ++ " "
        ++ (predicate |> predicateToString)
        ++ (Punctuation punctuation |> BaseWord |> wordToString)


isThirdPerson : Subject -> Bool
isThirdPerson subject =
    case subject of
        PronounSubject pronoun ->
            case pronoun |> toSubject of
                He ->
                    True

                She ->
                    True

                It ->
                    True

                _ ->
                    False

        NounSubject noun ->
            case noun of
                IndefiniteNoun _ _ ->
                    True

                DefiniteNoun _ _ ->
                    True

                UncountableNoun _ ->
                    True

                DefiniteUncountableNoun _ ->
                    True

                ProperNoun _ ->
                    True

                _ ->
                    False

        _ ->
            False


subjectToWord : Subject -> Word
subjectToWord nounLike =
    case nounLike of
        NounSubject noun ->
            noun |> Noun

        PronounSubject pronoun ->
            pronoun |> toSubject |> Pronoun

        WrongSubject word ->
            word


wordToSubject : Word -> Subject
wordToSubject word =
    case word of
        Noun noun ->
            noun |> NounSubject

        Pronoun pronoun ->
            pronoun |> PronounSubject

        _ ->
            WrongSubject word


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


objectToSubject : Object -> Subject
objectToSubject nounLike =
    case nounLike of
        NounObject noun ->
            noun |> NounSubject

        PronounObject pronoun ->
            pronoun |> PronounSubject


type Predicate
    = Predicate (List Word)


transformVerb : (Verb -> Verb) -> Predicate -> Predicate
transformVerb transform (Predicate words) =
    case words of
        (Verb verb) :: xs ->
            (verb |> transform |> Verb) :: xs |> Predicate

        _ ->
            words |> Predicate


predicateToString : Predicate -> String
predicateToString (Predicate words) =
    words |> List.map (BaseWord >> wordToString) |> String.join " "


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
