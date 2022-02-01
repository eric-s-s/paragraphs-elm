module Groups exposing (..)
import Word exposing (..)
import WordData exposing (VerbData)
import Word exposing (Verb)
import WordData exposing (getBasicVerb)

type Subject
    = NounSubject Noun
    | PronounSubject Pronoun

subjectToWord: Subject -> Word
subjectToWord nounLike = 
    case nounLike of
        NounSubject noun -> noun |> Noun
        PronounSubject pronoun -> pronoun |> toSubject |> Pronoun
type Object
    = NounObject Noun
    | PronounObject Pronoun

objectToWord: Object -> Word
objectToWord nounLike = 
    case nounLike of
        NounObject noun -> noun |> Noun
        PronounObject pronoun -> pronoun |> toObject |> Pronoun

type Predicate = Predicate (List Word)

toPredicate : (Object, Object) -> VerbData -> Predicate
toPredicate (firstObject, secondObject) verbData = [getBasicVerb verbData |> Verb, firstObject |> objectToWord] |> Predicate
