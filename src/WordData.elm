module WordData exposing (..)

import Word exposing (BaseNoun(..), Infinitive(..), IrregularPast(..), IrregularPlural(..), Noun(..), Particle(..), Preposition(..), Verb(..))


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
    [ BasicNoun (BaseNoun "ant") NoIrregularPlural
    , BasicNoun (BaseNoun "apple") NoIrregularPlural
    , BasicNoun (BaseNoun "baby") NoIrregularPlural
    , BasicNoun (BaseNoun "banana") NoIrregularPlural
    , BasicNoun (BaseNoun "bicycle") NoIrregularPlural
    , BasicNoun (BaseNoun "book") NoIrregularPlural
    , BasicNoun (BaseNoun "box") NoIrregularPlural
    , BasicNoun (BaseNoun "bus") NoIrregularPlural
    , BasicNoun (BaseNoun "car") NoIrregularPlural
    , BasicNoun (BaseNoun "child") (IrregularPlural "children")
    , BasicNoun (BaseNoun "cow") NoIrregularPlural
    , BasicNoun (BaseNoun "eagle") NoIrregularPlural
    , BasicNoun (BaseNoun "egg") NoIrregularPlural
    , BasicNoun (BaseNoun "elephant") NoIrregularPlural
    , BasicNoun (BaseNoun "finger") NoIrregularPlural
    , BasicNoun (BaseNoun "fire fighter") NoIrregularPlural
    , BasicNoun (BaseNoun "fish") (IrregularPlural "fish")
    , BasicNoun (BaseNoun "house") NoIrregularPlural
    , BasicNoun (BaseNoun "husband") NoIrregularPlural
    , BasicNoun (BaseNoun "knife") NoIrregularPlural
    , BasicNoun (BaseNoun "leaf") NoIrregularPlural
    , BasicNoun (BaseNoun "nose") NoIrregularPlural
    , BasicNoun (BaseNoun "octopus") NoIrregularPlural
    , BasicNoun (BaseNoun "orange") NoIrregularPlural
    , BasicNoun (BaseNoun "pen") NoIrregularPlural
    , BasicNoun (BaseNoun "person") (IrregularPlural "people")
    , BasicNoun (BaseNoun "pineapple") NoIrregularPlural
    , BasicNoun (BaseNoun "pony") NoIrregularPlural
    , BasicNoun (BaseNoun "school") NoIrregularPlural
    , BasicNoun (BaseNoun "shark") NoIrregularPlural
    , BasicNoun (BaseNoun "sheep") (IrregularPlural "sheep")
    , BasicNoun (BaseNoun "table") NoIrregularPlural
    , BasicNoun (BaseNoun "teacher") NoIrregularPlural
    , BasicNoun (BaseNoun "tiger") NoIrregularPlural
    , BasicNoun (BaseNoun "tree") NoIrregularPlural
    , BasicNoun (BaseNoun "uncle") NoIrregularPlural
    , BasicNoun (BaseNoun "watch") NoIrregularPlural
    , BasicNoun (BaseNoun "wife") NoIrregularPlural
    , BasicNoun (BaseNoun "witch") NoIrregularPlural
    , UncountableNoun (BaseNoun "apple juice")
    , UncountableNoun (BaseNoun "cake")
    , UncountableNoun (BaseNoun "gold")
    , UncountableNoun (BaseNoun "hair")
    , UncountableNoun (BaseNoun "homework")
    , UncountableNoun (BaseNoun "ice cream")
    , UncountableNoun (BaseNoun "lightning")
    , UncountableNoun (BaseNoun "milk")
    , UncountableNoun (BaseNoun "money")
    , UncountableNoun (BaseNoun "pizza")
    , UncountableNoun (BaseNoun "poop")
    , UncountableNoun (BaseNoun "rice")
    , UncountableNoun (BaseNoun "sand")
    , UncountableNoun (BaseNoun "stinky tofu")
    , UncountableNoun (BaseNoun "tea")
    , UncountableNoun (BaseNoun "thunder")
    , UncountableNoun (BaseNoun "water")
    , ProperNoun (BaseNoun "Tom")
    , ProperNoun (BaseNoun "Dick")
    , ProperNoun (BaseNoun "Harry")
    , ProperNoun (BaseNoun "Bat Man")
    , ProperPluralNoun (BaseNoun "the Joneses")
    , ProperPluralNoun (BaseNoun "the Avengers")
    , ProperPluralNoun (BaseNoun "Ferraris")
    ]
