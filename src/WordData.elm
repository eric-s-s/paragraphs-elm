module WordData exposing (..)
import Word exposing (Verb(..))
import Word exposing (ParticleType(..))
import Word exposing (PrepositionType(..))
import Word exposing (Infinitive(..))
import Word exposing (IrregularPast(..))
import Word exposing (Word(..))

type NumberOfObjects = One | Two

type alias VerbData =
    { infinitive : String
    , irregularPast: Maybe String
    , numberOfObjects: NumberOfObjects
    , preposition: Maybe String
    , particle: Maybe String
    }

getBasicVerb : VerbData -> Verb
getBasicVerb datum = 
    case datum.irregularPast of
        Nothing -> BasicVerb (Infinitive datum.infinitive) NoIrregularPast
        Just irregular -> BasicVerb (Infinitive datum.infinitive) (IrregularPast irregular)


getPreposition: VerbData -> Maybe PrepositionType
getPreposition datum = Maybe.map PrepositionType datum.preposition

getParticle: VerbData -> Maybe ParticleType
getParticle datum = Maybe.map SeparableParticle datum.preposition

moreVerbs: List VerbData
moreVerbs = 
    [ VerbData "bite" (Just "bit") Two Nothing Nothing
    , VerbData "bore" Nothing One Nothing Nothing
    , VerbData "bring" (Just "brought") Two Nothing Nothing
    , VerbData "bring" (Just "brought") Two (Just "to") Nothing
    , VerbData "clean" Nothing One Nothing (Just "up")
    , VerbData "clean" Nothing Two (Just "with") (Just "up")

    ]
