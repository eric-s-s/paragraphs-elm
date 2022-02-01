module Word exposing (..)

import Char exposing (toUpper)
import List exposing (any)
import String exposing (contains, dropRight, endsWith, left, right)


type PrepositionType = PrepositionType String

type ParticleType = SeparableParticle String

type Word
    = Noun Noun
    | Verb Verb
    | Punctuation Punctuation
    | Pronoun Pronoun
    | BeVerb BeVerb
    | NegativeBeVerb BeVerb
    | Preposition PrepositionType
    | Particle ParticleType


type RawValue
    = RawValue String


getString =
    \(RawValue el) -> el


wordToValue : Word -> RawValue
wordToValue word =
    case word of
        Noun x ->
            nounToRawValue x

        Verb x ->
            verbToRawValue x

        Punctuation x ->
            punctuationToRawValue x

        Pronoun x ->
            pronounToRawValue x

        BeVerb x ->
            beVerbToRawValue x

        NegativeBeVerb x ->
            (x |> beVerbToRawValue >> getString)
                ++ " not"
                |> RawValue

        Preposition (PrepositionType x) ->
            x |> RawValue

        Particle (SeparableParticle x) ->
            x |> RawValue


type FormattedWord
    = BaseWord Word
    | Bold Word
    | CapitalOoooooops Word
    | BoldCapital Word


type Punctuation
    = Period
    | ExclamationPoint
    | QuestionMark
    | Comma


punctuationToRawValue : Punctuation -> RawValue
punctuationToRawValue punctuation =
    case punctuation of
        Period ->
            "." |> RawValue

        ExclamationPoint ->
            "!" |> RawValue

        QuestionMark ->
            "?" |> RawValue

        Comma ->
            "," |> RawValue


type BeVerb
    = Is
    | Am
    | Are
    | Was
    | Were


beVerbToRawValue : BeVerb -> RawValue
beVerbToRawValue beVerb =
    case beVerb of
        Is ->
            "is" |> RawValue

        Am ->
            "am" |> RawValue

        Are ->
            "are" |> RawValue

        Was ->
            "was" |> RawValue

        Were ->
            "were" |> RawValue


beVerbToPast : BeVerb -> BeVerb
beVerbToPast beVerb =
    case beVerb of
        Is ->
            Was

        Am ->
            Was

        Are ->
            Were

        _ ->
            beVerb


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
    = NoIrregularPlural
    | IrregularPlural String


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

        IndefiniteNoun _ _ ->
            noun

        PluralNoun _ _ ->
            IncorrectNoun (noun |> nounToIndefiniteStringHelper) noun

        DefinitePluralNoun a b ->
            IncorrectNoun (PluralNoun a b |> nounToIndefiniteStringHelper) noun

        _ ->
            IncorrectNoun (toOriginalNoun noun |> nounToIndefiniteStringHelper) noun


toPlural : Noun -> Noun
toPlural noun =
    case noun of
        BasicNoun a b ->
            PluralNoun a b

        DefiniteNoun a b ->
            DefinitePluralNoun a b

        PluralNoun _ _ ->
            noun

        DefinitePluralNoun _ _ ->
            noun

        ProperPluralNoun _ ->
            noun

        _ ->
            IncorrectNoun (noun |> nounToPluralStringHelper) noun


nounToStringHelper =
    nounToRawValue >> getString


nounToPluralStringHelper =
    addPlural << nounToStringHelper


nounToIndefiniteStringHelper =
    addIndefinteArticle << nounToStringHelper


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

        PluralNoun _ (IrregularPlural value) ->
            value |> RawValue

        PluralNoun (BaseNoun value) _ ->
            addPlural value |> RawValue

        DefinitePluralNoun _ (IrregularPlural value) ->
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


type Infinitive
    = Infinitive String


type IrregularPast
    = NoIrregularPast
    | IrregularPast String


type Verb
    = IncorrectVerb String Verb
    | BasicVerb Infinitive IrregularPast
    | Negative Infinitive IrregularPast
    | ThirdPerson Infinitive IrregularPast
    | ThirdPersonNegative Infinitive IrregularPast
    | Past Infinitive IrregularPast
    | PastNegative Infinitive IrregularPast


makeBasicVerb : String -> Maybe String -> Verb
makeBasicVerb base past = 
    case past of
        Just irregular -> BasicVerb (Infinitive base) (IrregularPast irregular)
        Nothing -> BasicVerb (Infinitive base) NoIrregularPast

toOriginalVerb : Verb -> Verb
toOriginalVerb verb =
    case verb of
        BasicVerb a b ->
            BasicVerb a b

        Negative a b ->
            BasicVerb a b

        ThirdPerson a b ->
            BasicVerb a b

        ThirdPersonNegative a b ->
            BasicVerb a b

        Past a b ->
            BasicVerb a b

        PastNegative a b ->
            BasicVerb a b

        IncorrectVerb _ other ->
            toOriginalVerb other


toThirdPerson : Verb -> Verb
toThirdPerson verb =
    case verb of
        BasicVerb a b ->
            ThirdPerson a b

        Negative a b ->
            ThirdPersonNegative a b

        Past _ _ ->
            IncorrectVerb (verb |> verbToStringHelper >> addS) verb

        PastNegative _ _ ->
            IncorrectVerb (verb |> verbToStringHelper >> addS) verb

        _ ->
            verb


toNegative : Verb -> Verb
toNegative verb =
    case verb of
        BasicVerb a b ->
            Negative a b

        ThirdPerson a b ->
            ThirdPersonNegative a b

        Past a b ->
            PastNegative a b

        _ ->
            verb


toPast : Verb -> Verb
toPast verb =
    case verb of
        BasicVerb a b ->
            Past a b

        Negative a b ->
            PastNegative a b

        ThirdPerson _ _ ->
            IncorrectVerb (verb |> toOriginalVerb >> toPast >> verbToStringHelper >> addS) verb

        ThirdPersonNegative _ _ ->
            IncorrectVerb
                ("doesn't "
                    ++ (verbToStringHelper << toPast << toOriginalVerb <| verb)
                )
                verb

        _ ->
            verb


verbToStringHelper =
    verbToRawValue >> getString


verbToRawValue : Verb -> RawValue
verbToRawValue verb =
    case verb of
        IncorrectVerb value _ ->
            value |> RawValue

        BasicVerb (Infinitive value) _ ->
            value |> RawValue

        Negative (Infinitive value) _ ->
            "don't " ++ value |> RawValue

        ThirdPerson (Infinitive "have") _ ->
            "has" |> RawValue

        ThirdPerson (Infinitive value) _ ->
            addS value |> RawValue

        ThirdPersonNegative (Infinitive value) _ ->
            "doesn't " ++ value |> RawValue

        Past _ (IrregularPast value) ->
            value |> RawValue

        Past (Infinitive value) _ ->
            addEd value |> RawValue

        PastNegative (Infinitive value) _ ->
            "didn't " ++ value |> RawValue


addEd : String -> String
addEd word =
    if isYLongVowel word then
        dropRight 1 word ++ "ied"

    else if endsWithShortVowelAndConsonant word then
        word ++ right 1 word ++ "ed"

    else if endsWith "e" word then
        word ++ "d"

    else
        word ++ "ed"


endsWithShortVowelAndConsonant : String -> Bool
endsWithShortVowelAndConsonant word =
    (isCharAtIndexAVowel -2 <| word)
        && not (isCharAtIndexAVowelPlus -1 <| word)
        && not (isCharAtIndexAVowel -3 <| word)


isCharAtIndexAVowel =
    isCharAtIndexInPool "aeiou"


isCharAtIndexAVowelPlus =
    isCharAtIndexInPool "aeiouyw"


isCharAtIndexInPool : String -> Int -> String -> Bool
isCharAtIndexInPool pool index toTest =
    case getCharAtIndex index toTest of
        Nothing ->
            False

        Just char ->
            String.toList pool |> List.member char


getCharAtIndex : Int -> String -> Maybe Char
getCharAtIndex index word =
    if index == -1 then
        right 1 word |> String.toList |> List.head

    else
        String.slice index (index + 1) word |> String.toList |> List.head


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
        && not (List.member (right 2 word) endings)


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
