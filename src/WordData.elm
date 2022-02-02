module WordData exposing (..)

import Word exposing (Infinitive(..), IrregularPast(..), ParticleType(..), PrepositionType(..), Verb(..), Word(..))


type NumberOfObjects
    = One
    | Two


type alias VerbData =
    { infinitive : String
    , irregularPast : Maybe String
    , numberOfObjects : NumberOfObjects
    , particle : Maybe String
    , preposition : Maybe String
    }


type VerbDataTwo
    = SingleObject Verb
    | SingleObjectParticle ParticleType Verb
    | SingleObjectPreposition PrepositionType Verb
    | SingleObjectParticlePreposition Verb ParticleType PrepositionType
    | DoubleObject Verb
    | DoubleObjectPreposition Verb PrepositionType
    | DoubleObjectParticlePreposition Verb ParticleType PrepositionType


getBasicVerb : VerbData -> Verb
getBasicVerb datum =
    case datum.irregularPast of
        Nothing ->
            BasicVerb (Infinitive datum.infinitive) NoIrregularPast

        Just irregular ->
            BasicVerb (Infinitive datum.infinitive) (IrregularPast irregular)


getPreposition : VerbData -> Maybe PrepositionType
getPreposition datum =
    Maybe.map PrepositionType datum.preposition


getParticle : VerbData -> Maybe ParticleType
getParticle datum =
    Maybe.map SeparableParticle datum.preposition


moreVerbs : List VerbData
moreVerbs =
    [ VerbData "bite" (Just "bit") Two Nothing Nothing
    , VerbData "bore" Nothing One Nothing Nothing
    , VerbData "break" (Just "broke") One Nothing Nothing
    , VerbData "bring" (Just "brought") Two Nothing Nothing
    , VerbData "bring" (Just "brought") Two Nothing (Just "to")
    , VerbData "build" (Just "built") One Nothing Nothing
    , VerbData "buy" (Just "bought") One Nothing Nothing
    , VerbData "catch" (Just "caught") One Nothing Nothing
    , VerbData "clean" Nothing One (Just "up") Nothing
    , VerbData "clean" Nothing Two (Just "up") (Just "with")
    , VerbData "cook" Nothing One Nothing Nothing
    , VerbData "cut" (Just "cut") One Nothing Nothing
    , VerbData "cut" (Just "cut") Two Nothing (Just "with")
    , VerbData "disgust" Nothing One Nothing Nothing
    , VerbData "draw" (Just "drew") One Nothing Nothing
    , VerbData "eat" (Just "ate") One Nothing Nothing
    , VerbData "fall" (Just "fell") One Nothing (Just "on")
    , VerbData "feed" (Just "fed") One Nothing Nothing
    , VerbData "fight" (Just "fought") One Nothing Nothing
    , VerbData "find" (Just "found") One Nothing Nothing
    , VerbData "freeze" (Just "froze") One Nothing Nothing
    , VerbData "give" (Just "gave") Two Nothing Nothing
    , VerbData "grab" Nothing One Nothing Nothing
    , VerbData "hate" Nothing One Nothing Nothing
    , VerbData "have" (Just "had") One Nothing Nothing
    , VerbData "hit" (Just "hit") One Nothing Nothing
    , VerbData "hit" (Just "hit") Two Nothing (Just "with")
    , VerbData "hold" (Just "held") One Nothing Nothing
    , VerbData "interest" Nothing One Nothing Nothing
    , VerbData "jump" Nothing One Nothing (Just "on")
    , VerbData "kick" Nothing One Nothing Nothing
    , VerbData "kill" Nothing One Nothing Nothing
    , VerbData "kiss" Nothing One Nothing Nothing
    , VerbData "like" Nothing One Nothing Nothing
    , VerbData "love" Nothing One Nothing Nothing
    , VerbData "make" (Just "made") One Nothing Nothing
    , VerbData "make" (Just "made") Two Nothing (Just "out of")
    , VerbData "marry" Nothing One Nothing Nothing
    , VerbData "own" Nothing One Nothing Nothing
    , VerbData "pick" Nothing One (Just "up") Nothing
    , VerbData "put" (Just "put") One (Just "away") Nothing
    , VerbData "play" Nothing One Nothing (Just "with")
    , VerbData "pull" Nothing One Nothing Nothing
    , VerbData "push" Nothing One Nothing Nothing
    , VerbData "ride" (Just "rode") One Nothing Nothing
    , VerbData "see" (Just "saw") One Nothing Nothing
    , VerbData "sell" (Just "sold") One Nothing Nothing
    , VerbData "sell" (Just "sold") Two Nothing (Just "to")
    , VerbData "shake" (Just "shook") One Nothing Nothing
    , VerbData "show" Nothing Two Nothing Nothing
    , VerbData "sleep" (Just "slept") One Nothing (Just "on")
    , VerbData "smell" Nothing One Nothing Nothing
    , VerbData "steal" (Just "stole") One Nothing Nothing
    , VerbData "surprise" Nothing One Nothing Nothing
    , VerbData "take" (Just "took") One Nothing Nothing
    , VerbData "teach" (Just "taught") Two Nothing (Just "about")
    , VerbData "throw" (Just "threw") One Nothing Nothing
    , VerbData "throw" (Just "threw") Two Nothing (Just "at")
    , VerbData "use" Nothing One Nothing Nothing
    , VerbData "wash" Nothing One Nothing Nothing
    , VerbData "wear" (Just "wore") One Nothing Nothing
    ]
