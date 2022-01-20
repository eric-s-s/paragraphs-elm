module Words.Word exposing (..)
import Tags exposing (WordTag(..))
import String exposing (endsWith)
import String exposing (dropRight)
import List exposing (any)
import List exposing (length)
import List exposing (member)
import String exposing (left)
import String exposing (contains)
import Dict exposing (values)


type Capitalized = Capital | Lowercase

type BasePronoun
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

toObject : BasePronoun -> BasePronoun
toObject base = 
    case base of
        I -> Me
        He -> Him
        She -> Her
        We -> Us
        They -> Them
        _ -> base

toSubject : BasePronoun -> BasePronoun
toSubject base = 
    case base of
        Me -> I
        Him -> He
        Her -> She
        Us -> We
        Them -> They
        _ -> base



toValue : BasePronoun -> String
toValue value = 
  case value of
    I -> "I"
    Me -> "Me"
    You -> "You"
    He -> "He"
    Him -> "Him"
    She -> "She"
    Her -> "Her"
    It -> "It"
    We -> "We"
    Us -> "Us"
    They -> "They"
    Them -> "Them"

type Word = 
  Pronoun BasePronoun Capitalized

toString : Word -> String
toString value = 
  case value of
    Pronoun base Capital -> toValue base
    Pronoun I Lowercase -> "I"
    Pronoun base Lowercase -> String.toLower <| toValue base

type BaseValue = BaseValue String
type IrregularPlural = IrregularPlural (Maybe String)

type Noun
    = BaseNoun BaseValue IrregularPlural
    | IndefiniteNoun BaseValue IrregularPlural
    | DefiniteNoun BaseValue IrregularPlural
    | PluralNoun BaseValue IrregularPlural
    | DefinitePluralNoun BaseValue IrregularPlural
    | UncountableNoun BaseValue
    | DefiniteUncountableNoun BaseValue
    | ProperNoun BaseValue
    | ProperPluralNoun BaseValue
    | IncorrectNoun String Noun

toNounValue : Noun -> String
toNounValue noun = 
    case noun of
        IncorrectNoun value _ -> value
        BaseNoun (BaseValue value) _ -> value
        IndefiniteNoun (BaseValue value) _ -> addIndefinteArticle value
        DefiniteNoun (BaseValue value) _ -> "the " ++ value
        PluralNoun _ (IrregularPlural (Just value)) -> value
        PluralNoun (BaseValue value) _ -> addPlural value
        DefinitePluralNoun _ (IrregularPlural (Just value)) -> "the " ++ value
        DefinitePluralNoun (BaseValue value) _ -> "the " ++ addPlural value
        UncountableNoun (BaseValue value) -> value
        DefiniteUncountableNoun (BaseValue value) -> "the " ++ value
        ProperNoun (BaseValue value) -> value
        ProperPluralNoun (BaseValue value) -> value
        
addIndefinteArticle: String -> String
addIndefinteArticle word = 
    if contains (left 1 word) "aeiouAEIOU" then
        "an " ++ word
    else
        "a " ++ word

addPlural: String -> String
addPlural word = 
    let 
        vEndings = ["alf", "elf", "arf", "eaf", "oaf", "olf"]
    in
    if endsWith "ife" word then 
        (dropRight 3 word) ++ "ives"
    else if (any (\el -> endsWith el word) vEndings) then
        (dropRight 1 word) ++ "ves"
    else
        addS word

isYLongVowel : String -> Bool
isYLongVowel word = 
    let
        endings = ["ay", "ey", "iy", "oy", "uy"]
    in
    endsWith "y" word && 
    String.length word > 1 && 
    not (List.member (String.right 2 word) endings)

needsEs : String -> Bool
needsEs word = any (\el -> endsWith el word) ["s", "z", "ch", "sh", "x", "o"]

addS : String -> String
addS word =
    if needsEs word then
        word ++ "es"
    else if isYLongVowel word then
        (dropRight 1 word) ++ "ies"
    else
        word ++ "s"

