module Paragraphs exposing (..)

import Html exposing (sub)
import List exposing (filter, filterMap)
import Sentences exposing (Predicate(..), Sentence(..), Subject(..), sentenceToString, subjectToWord)
import Word exposing (Noun(..), Word(..), toDefinite, toIndefinite)


type Paragraph
    = UnformattedParagraph (List Sentence)
    | NounFormattedParagraph (List Sentence)
    | FormattedParagraph (List Sentence)


paragraphToString : Paragraph -> String
paragraphToString paragraph =
    case paragraph of
        NounFormattedParagraph sentences ->
            String.join " " (sentences |> List.map sentenceToString)

        _ ->
            ""


formatNouns : Paragraph -> Paragraph
formatNouns paragraph =
    case paragraph of
        UnformattedParagraph sentences ->
            NounFormattedParagraph (sentences |> List.map makeSubjectIndefinite)

        _ ->
            NounFormattedParagraph []


makeSubjectIndefinite : Sentence -> Sentence
makeSubjectIndefinite sentence =
    case sentence of
        SimplePresent subject predicate punctuation ->
            SimplePresent
                (subject |> subjectToIndefiniteSubject)
                predicate
                punctuation

        NegativeSimplePresent subject predicate punctuation ->
            NegativeSimplePresent
                (subject |> subjectToIndefiniteSubject)
                predicate
                punctuation

        SimplePast subject predicate punctuation ->
            SimplePast
                (subject |> subjectToIndefiniteSubject)
                predicate
                punctuation

        NegativeSimplePast subject predicate punctuation ->
            NegativeSimplePast
                (subject |> subjectToIndefiniteSubject)
                predicate
                punctuation


subjectToIndefiniteSubject : Subject -> Subject
subjectToIndefiniteSubject subject =
    case subject of
        NounSubject noun ->
            case noun of
                RawCountableNoun _ _ ->
                    noun |> toIndefinite |> NounSubject

                _ ->
                    subject

        _ ->
            subject


getList : Predicate -> List Word
getList (Predicate words) =
    words


getNouns : Sentence -> List Noun
getNouns sentence =
    []


getNounsFromPredicate : Predicate -> List Noun
getNounsFromPredicate (Predicate word) =
    []


allowsDefiniteArticle : Noun -> Bool
allowsDefiniteArticle noun =
    case noun of
        RawCountableNoun _ _ ->
            True

        IndefiniteNoun _ _ ->
            True

        UncountableNoun _ ->
            True

        PluralNoun _ _ ->
            True

        _ ->
            False


wordToNoun : Word -> Maybe Noun
wordToNoun word =
    case word of
        Noun noun ->
            Just noun

        _ ->
            Nothing


sentenceToWordList : Sentence -> List Word
sentenceToWordList sentence =
    case sentence of
        SimplePresent subject (Predicate words) punctuation ->
            subjectToWord subject :: words ++ [ punctuation |> Punctuation ]

        NegativeSimplePresent subject (Predicate words) punctuation ->
            subjectToWord subject :: words ++ [ punctuation |> Punctuation ]

        SimplePast subject (Predicate words) punctuation ->
            subjectToWord subject :: words ++ [ punctuation |> Punctuation ]

        NegativeSimplePast subject (Predicate words) punctuation ->
            subjectToWord subject :: words ++ [ punctuation |> Punctuation ]


getDefiniteCandidates : Sentence -> List Noun
getDefiniteCandidates sentence =
    sentence |> sentenceToWordList |> filterMap wordToNoun |> filter allowsDefiniteArticle


transformToDefinite : List Noun -> List Sentence -> List Sentence
transformToDefinite definiteNouns sentences =
    []
