module Paragraphs exposing (..)

import Html exposing (sub)
import List exposing (filter, filterMap)
import Sentences exposing (Predicate(..), Sentence(..), Subject(..), sentenceMap, sentenceToString, subjectToWord)
import Word exposing (Noun(..), Word(..), toDefinite, toIndefinite, toOriginalNoun)


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
            NounFormattedParagraph
                (sentences
                    |> List.map makeIndefinites
                    |> changeRepeatedNounsToDefinite []
                )

        _ ->
            NounFormattedParagraph []


makeIndefinites : Sentence -> Sentence
makeIndefinites sentence =
    sentenceMap safeIndefinite sentence


safeIndefinite : Word -> Word
safeIndefinite word =
    case word of
        Noun noun ->
            case noun of
                RawCountableNoun _ _ ->
                    noun |> toIndefinite |> Noun

                _ ->
                    word

        _ ->
            word


changeRepeatedNounsToDefinite : List Noun -> List Sentence -> List Sentence
changeRepeatedNounsToDefinite repeatedNouns sentences =
    case sentences of
        [] ->
            []

        first :: others ->
            changeMarkedToDefinite repeatedNouns first
                :: changeRepeatedNounsToDefinite
                    (repeatedNouns ++ getDefiniteCandidates first)
                    others


changeMarkedToDefinite : List Noun -> Sentence -> Sentence
changeMarkedToDefinite markedNouns sentence =
    sentenceMap (makeDefiniteIfMarked markedNouns) sentence


makeDefiniteIfMarked : List Noun -> Word -> Word
makeDefiniteIfMarked markedNouns word =
    case word of
        Noun noun ->
            if List.member noun markedNouns then
                noun |> toDefinite |> Noun

            else
                word

        _ ->
            word


getDefiniteCandidates : Sentence -> List Noun
getDefiniteCandidates sentence =
    sentence |> sentenceToWordList |> filterMap wordToNoun |> filter allowsDefiniteArticle


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
