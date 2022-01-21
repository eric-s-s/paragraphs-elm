module Words.Word exposing (..)

import Char exposing (toUpper)
import Dict exposing (values)
import List exposing (any, length, member)
import String exposing (contains, dropRight, endsWith, left)
import Tags exposing (WordTag(..))


type Word
    = Pronoun Pronoun
    | Noun Noun


type RawValue
    = RawValue String


wordToValue : Word -> RawValue
wordToValue word =
    case word of
        Pronoun x ->
            pronounToRawValue x

        Noun x ->
            nounToRawValue x



-- _ -> RawValue "hello"


type FormattedWord
    = BaseWord Word
    | Bold Word
    | CapitalOoooooops Word
    | BoldCapital Word


type Pronoun
    = I
    | Me
    | You
    | He
    | Him
    | She
    | Her
    | It
    | We
    | Us
    | They
    | Them


toObject : Pronoun -> Pronoun
toObject base =
    case base of
        I ->
            Me

        He ->
            Him

        She ->
            Her

        We ->
            Us

        They ->
            Them

        _ ->
            base


toSubject : Pronoun -> Pronoun
toSubject base =
    case base of
        Me ->
            I

        Him ->
            He

        Her ->
            She

        Us ->
            We

        Them ->
            They

        _ ->
            base


pronounToRawValue : Pronoun -> RawValue
pronounToRawValue value =
    case value of
        I ->
            "I" |> RawValue

        Me ->
            "me" |> RawValue

        You ->
            "you" |> RawValue

        He ->
            "he" |> RawValue

        Him ->
            "him" |> RawValue

        She ->
            "she" |> RawValue

        Her ->
            "her" |> RawValue

        It ->
            "it" |> RawValue

        We ->
            "we" |> RawValue

        Us ->
            "us" |> RawValue

        They ->
            "they" |> RawValue

        Them ->
            "them" |> RawValue


type BaseNoun
    = BaseNoun String


type IrregularPlural
    = IrregularPlural (Maybe String)


type Noun
    = BasicNoun BaseNoun IrregularPlural
    | IndefiniteNoun BaseNoun IrregularPlural
    | DefiniteNoun BaseNoun IrregularPlural
    | PluralNoun BaseNoun IrregularPlural
    | DefinitePluralNoun BaseNoun IrregularPlural
    | UncountableNoun BaseNoun
    | DefiniteUncountableNoun BaseNoun
    | ProperNoun BaseNoun
    | ProperPluralNoun BaseNoun
    | IncorrectNoun String Noun


toOriginalNoun : Noun -> Noun
toOriginalNoun noun =
    case noun of
        IndefiniteNoun a b ->
            BasicNoun a b

        DefiniteNoun a b ->
            BasicNoun a b

        PluralNoun a b ->
            BasicNoun a b

        DefinitePluralNoun a b ->
            BasicNoun a b

        DefiniteUncountableNoun a ->
            UncountableNoun a

        IncorrectNoun _ a ->
            toOriginalNoun a

        _ ->
            noun


toDefinite : Noun -> Noun
toDefinite noun =
    case noun of
        BasicNoun a b ->
            DefiniteNoun a b

        IndefiniteNoun a b ->
            DefiniteNoun a b

        PluralNoun a b ->
            DefinitePluralNoun a b

        UncountableNoun a ->
            DefiniteUncountableNoun a

        ProperNoun (BaseNoun a) ->
            IncorrectNoun ("the " ++ a) noun

        ProperPluralNoun (BaseNoun a) ->
            IncorrectNoun ("the " ++ a) noun

        IncorrectNoun value original ->
            IncorrectNoun ("the " ++ value) original

        _ ->
            noun


toIndefinite : Noun -> Noun
toIndefinite noun =
    case noun of
        BasicNoun a b ->
            IndefiniteNoun a b

        DefiniteNoun a b ->
            IndefiniteNoun a b

        PluralNoun _ _ ->
            IncorrectNoun
                (noun
                    |> addIndefinteArticle
                    << (\(RawValue str) -> str)
                    << nounToRawValue
                )
                noun

        _ ->
            noun


nounToRawValue : Noun -> RawValue
nounToRawValue noun =
    case noun of
        IncorrectNoun value _ ->
            value |> RawValue

        BasicNoun (BaseNoun value) _ ->
            value |> RawValue

        IndefiniteNoun (BaseNoun value) _ ->
            addIndefinteArticle value |> RawValue

        DefiniteNoun (BaseNoun value) _ ->
            "the " ++ value |> RawValue

        PluralNoun _ (IrregularPlural (Just value)) ->
            value |> RawValue

        PluralNoun (BaseNoun value) _ ->
            addPlural value |> RawValue

        DefinitePluralNoun _ (IrregularPlural (Just value)) ->
            "the " ++ value |> RawValue

        DefinitePluralNoun (BaseNoun value) _ ->
            "the " ++ addPlural value |> RawValue

        UncountableNoun (BaseNoun value) ->
            value |> RawValue

        DefiniteUncountableNoun (BaseNoun value) ->
            "the " ++ value |> RawValue

        ProperNoun (BaseNoun value) ->
            value |> RawValue

        ProperPluralNoun (BaseNoun value) ->
            value |> RawValue


addIndefinteArticle : String -> String
addIndefinteArticle word =
    if contains (left 1 word) "aeiouAEIOU" then
        "an " ++ word

    else
        "a " ++ word


addPlural : String -> String
addPlural word =
    let
        vEndings =
            [ "alf", "elf", "arf", "eaf", "oaf", "olf" ]
    in
    if endsWith "ife" word then
        dropRight 3 word ++ "ives"

    else if any (\el -> endsWith el word) vEndings then
        dropRight 1 word ++ "ves"

    else
        addS word


addS : String -> String
addS word =
    if needsEs word then
        word ++ "es"

    else if isYLongVowel word then
        dropRight 1 word ++ "ies"

    else
        word ++ "s"


isYLongVowel : String -> Bool
isYLongVowel word =
    let
        endings =
            [ "ay", "ey", "iy", "oy", "uy" ]
    in
    endsWith "y" word
        && String.length word
        > 1
        && not (List.member (String.right 2 word) endings)


needsEs : String -> Bool
needsEs word =
    any (\el -> endsWith el word) [ "s", "z", "ch", "sh", "x", "o" ]


capitalize : String -> String
capitalize word =
    case String.toList word of
        [] ->
            ""

        char :: chars ->
            toUpper char :: chars |> String.fromList
