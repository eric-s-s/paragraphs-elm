module WordData exposing (..)

import Word
    exposing
        ( Infinitive(..)
        , IrregularPast(..)
        , IrregularPlural(..)
        , Noun(..)
        , NounBase(..)
        , Particle(..)
        , Preposition(..)
        , Verb(..)
        )


type NumberOfObjects
    = One
    | Two


type alias VerbData =
    { infinitive : String
    , irregularPast : IrregularPast
    , numberOfObjects : NumberOfObjects
    , particle : Maybe Particle
    , preposition : Maybe Preposition
    }


getBasicVerb : VerbData -> Verb
getBasicVerb datum =
    BasicVerb (Infinitive datum.infinitive) datum.irregularPast


lotsaVerbs : List VerbData
lotsaVerbs =
    [ VerbData "bite" (IrregularPast "bit") Two Nothing Nothing
    , VerbData "bore" NoIrregularPast One Nothing Nothing
    , VerbData "break" (IrregularPast "broke") One Nothing Nothing
    , VerbData "bring" (IrregularPast "brought") Two Nothing Nothing
    , VerbData "bring" (IrregularPast "brought") Two Nothing (SimplePreposition "to" |> Just)
    , VerbData "build" (IrregularPast "built") One Nothing Nothing
    , VerbData "buy" (IrregularPast "bought") One Nothing Nothing
    , VerbData "catch" (IrregularPast "caught") One Nothing Nothing
    , VerbData "clean" NoIrregularPast One (AdverbialParticle "up" |> Just) Nothing
    , VerbData "clean" NoIrregularPast Two (AdverbialParticle "up" |> Just) (SimplePreposition "with" |> Just)
    , VerbData "cook" NoIrregularPast One Nothing Nothing
    , VerbData "cut" (IrregularPast "cut") One Nothing Nothing
    , VerbData "cut" (IrregularPast "cut") Two Nothing (SimplePreposition "with" |> Just)
    , VerbData "disgust" NoIrregularPast One Nothing Nothing
    , VerbData "draw" (IrregularPast "drew") One Nothing Nothing
    , VerbData "eat" (IrregularPast "ate") One Nothing Nothing
    , VerbData "fall" (IrregularPast "fell") One Nothing (SimplePreposition "on" |> Just)
    , VerbData "feed" (IrregularPast "fed") One Nothing Nothing
    , VerbData "fight" (IrregularPast "fought") One Nothing Nothing
    , VerbData "find" (IrregularPast "found") One Nothing Nothing
    , VerbData "freeze" (IrregularPast "froze") One Nothing Nothing
    , VerbData "give" (IrregularPast "gave") Two Nothing Nothing
    , VerbData "grab" NoIrregularPast One Nothing Nothing
    , VerbData "hate" NoIrregularPast One Nothing Nothing
    , VerbData "have" (IrregularPast "had") One Nothing Nothing
    , VerbData "hit" (IrregularPast "hit") One Nothing Nothing
    , VerbData "hit" (IrregularPast "hit") Two Nothing (SimplePreposition "with" |> Just)
    , VerbData "hold" (IrregularPast "held") One Nothing Nothing
    , VerbData "interest" NoIrregularPast One Nothing Nothing
    , VerbData "jump" NoIrregularPast One Nothing (SimplePreposition "on" |> Just)
    , VerbData "kick" NoIrregularPast One Nothing Nothing
    , VerbData "kill" NoIrregularPast One Nothing Nothing
    , VerbData "kiss" NoIrregularPast One Nothing Nothing
    , VerbData "like" NoIrregularPast One Nothing Nothing
    , VerbData "love" NoIrregularPast One Nothing Nothing
    , VerbData "make" (IrregularPast "made") One Nothing Nothing
    , VerbData "make" (IrregularPast "made") Two Nothing (SimplePreposition "out of" |> Just)
    , VerbData "marry" NoIrregularPast One Nothing Nothing
    , VerbData "own" NoIrregularPast One Nothing Nothing
    , VerbData "pick" NoIrregularPast One (AdverbialParticle "up" |> Just) Nothing
    , VerbData "put" (IrregularPast "put") One (AdverbialParticle "away" |> Just) Nothing
    , VerbData "play" NoIrregularPast One Nothing (SimplePreposition "with" |> Just)
    , VerbData "pull" NoIrregularPast One Nothing Nothing
    , VerbData "push" NoIrregularPast One Nothing Nothing
    , VerbData "ride" (IrregularPast "rode") One Nothing Nothing
    , VerbData "see" (IrregularPast "saw") One Nothing Nothing
    , VerbData "sell" (IrregularPast "sold") One Nothing Nothing
    , VerbData "sell" (IrregularPast "sold") Two Nothing (SimplePreposition "to" |> Just)
    , VerbData "shake" (IrregularPast "shook") One Nothing Nothing
    , VerbData "show" NoIrregularPast Two Nothing Nothing
    , VerbData "sleep" (IrregularPast "slept") One Nothing (SimplePreposition "on" |> Just)
    , VerbData "smell" NoIrregularPast One Nothing Nothing
    , VerbData "steal" (IrregularPast "stole") One Nothing Nothing
    , VerbData "surprise" NoIrregularPast One Nothing Nothing
    , VerbData "take" (IrregularPast "took") One Nothing Nothing
    , VerbData "teach" (IrregularPast "taught") Two Nothing (SimplePreposition "about" |> Just)
    , VerbData "throw" (IrregularPast "threw") One Nothing Nothing
    , VerbData "throw" (IrregularPast "threw") Two Nothing (SimplePreposition "at" |> Just)
    , VerbData "use" NoIrregularPast One Nothing Nothing
    , VerbData "wash" NoIrregularPast One Nothing Nothing
    , VerbData "wear" (IrregularPast "wore") One Nothing Nothing
    ]


lotsaNouns : List Noun
lotsaNouns =
    [ RawCountableNoun (NounBase "ant") NoIrregularPlural
    , RawCountableNoun (NounBase "apple") NoIrregularPlural
    , RawCountableNoun (NounBase "baby") NoIrregularPlural
    , RawCountableNoun (NounBase "banana") NoIrregularPlural
    , RawCountableNoun (NounBase "bicycle") NoIrregularPlural
    , RawCountableNoun (NounBase "book") NoIrregularPlural
    , RawCountableNoun (NounBase "box") NoIrregularPlural
    , RawCountableNoun (NounBase "bus") NoIrregularPlural
    , RawCountableNoun (NounBase "car") NoIrregularPlural
    , RawCountableNoun (NounBase "child") (IrregularPlural "children")
    , RawCountableNoun (NounBase "cow") NoIrregularPlural
    , RawCountableNoun (NounBase "eagle") NoIrregularPlural
    , RawCountableNoun (NounBase "egg") NoIrregularPlural
    , RawCountableNoun (NounBase "elephant") NoIrregularPlural
    , RawCountableNoun (NounBase "finger") NoIrregularPlural
    , RawCountableNoun (NounBase "fire fighter") NoIrregularPlural
    , RawCountableNoun (NounBase "fish") (IrregularPlural "fish")
    , RawCountableNoun (NounBase "house") NoIrregularPlural
    , RawCountableNoun (NounBase "husband") NoIrregularPlural
    , RawCountableNoun (NounBase "knife") NoIrregularPlural
    , RawCountableNoun (NounBase "leaf") NoIrregularPlural
    , RawCountableNoun (NounBase "nose") NoIrregularPlural
    , RawCountableNoun (NounBase "octopus") NoIrregularPlural
    , RawCountableNoun (NounBase "orange") NoIrregularPlural
    , RawCountableNoun (NounBase "pen") NoIrregularPlural
    , RawCountableNoun (NounBase "person") (IrregularPlural "people")
    , RawCountableNoun (NounBase "pineapple") NoIrregularPlural
    , RawCountableNoun (NounBase "pony") NoIrregularPlural
    , RawCountableNoun (NounBase "school") NoIrregularPlural
    , RawCountableNoun (NounBase "shark") NoIrregularPlural
    , RawCountableNoun (NounBase "sheep") (IrregularPlural "sheep")
    , RawCountableNoun (NounBase "table") NoIrregularPlural
    , RawCountableNoun (NounBase "teacher") NoIrregularPlural
    , RawCountableNoun (NounBase "tiger") NoIrregularPlural
    , RawCountableNoun (NounBase "tree") NoIrregularPlural
    , RawCountableNoun (NounBase "uncle") NoIrregularPlural
    , RawCountableNoun (NounBase "watch") NoIrregularPlural
    , RawCountableNoun (NounBase "wife") NoIrregularPlural
    , RawCountableNoun (NounBase "witch") NoIrregularPlural
    , UncountableNoun (NounBase "apple juice")
    , UncountableNoun (NounBase "cake")
    , UncountableNoun (NounBase "gold")
    , UncountableNoun (NounBase "hair")
    , UncountableNoun (NounBase "homework")
    , UncountableNoun (NounBase "ice cream")
    , UncountableNoun (NounBase "lightning")
    , UncountableNoun (NounBase "milk")
    , UncountableNoun (NounBase "money")
    , UncountableNoun (NounBase "pizza")
    , UncountableNoun (NounBase "poop")
    , UncountableNoun (NounBase "rice")
    , UncountableNoun (NounBase "sand")
    , UncountableNoun (NounBase "stinky tofu")
    , UncountableNoun (NounBase "tea")
    , UncountableNoun (NounBase "thunder")
    , UncountableNoun (NounBase "water")
    , ProperNoun (NounBase "Tom")
    , ProperNoun (NounBase "Dick")
    , ProperNoun (NounBase "Harry")
    , ProperNoun (NounBase "Batman")
    , ProperPluralNoun (NounBase "the Joneses")
    , ProperPluralNoun (NounBase "the Avengers")
    , ProperPluralNoun (NounBase "Ferraris")
    ]
