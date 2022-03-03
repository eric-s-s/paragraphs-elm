module Paragraphs exposing (..)

import Sentences exposing (Predicate(..), Sentence(..), Subject(..))
import Word exposing (Noun(..), Word(..), toDefinite)


type Paragraph
    = UnformattedParagraph (List Sentence)
    | FormattedParagraph (List Sentence)


getList : Predicate -> List Word
getList (Predicate words) =
    words


getNouns : Sentence -> List Noun
getNouns sentence =
    []


getNounsFromPredicate : Predicate -> List Noun
getNounsFromPredicate (Predicate word) =
    []


isNounThatNeedsAssignment : Word -> Bool
isNounThatNeedsAssignment word =
    case word of
        Noun noun ->
            case noun of
                RawCountableNoun _ _ ->
                    True

                DefiniteNoun _ _ ->
                    True

                _ ->
                    False

        _ ->
            False


transformToDefinite : List Noun -> List Word -> List Word
transformToDefinite definiteNouns words =
    case words of
        [] ->
            []

        (Noun noun) :: xs ->
            (noun |> Noun) :: transformToDefinite definiteNouns xs

        x :: xs ->
            x :: transformToDefinite definiteNouns xs
