module TestParagraphs exposing (..)

import Expect
import List exposing (map)
import Paragraphs
    exposing
        ( Paragraph(..)
        , formatNouns
        , paragraphToString
        )
import Sentences exposing (..)
import Test exposing (..)
import Word
    exposing
        ( Infinitive(..)
        , IrregularPast(..)
        , IrregularPlural(..)
        , Noun(..)
        , NounBase(..)
        , Particle(..)
        , Preposition(..)
        , Pronoun(..)
        , Punctuation(..)
        , Verb(..)
        , Word(..)
        , toIndefinite
        , toPlural
        , toSubject
        )
import WordData exposing (NumberOfObjects(..), VerbData)


giveVerbData : VerbData
giveVerbData =
    VerbData "give" (IrregularPast "gave") Two Nothing Nothing


seeVerbData : VerbData
seeVerbData =
    VerbData "see" (IrregularPast "saw") One Nothing Nothing


predicateWithGive : Object -> Object -> Predicate
predicateWithGive first second =
    toPredicate ( first, second ) giveVerbData


predicateWithSee : Object -> Predicate
predicateWithSee first =
    toPredicate ( first, first ) seeVerbData


dog : Noun
dog =
    RawCountableNoun (NounBase "dog") NoIrregularPlural


water : Noun
water =
    UncountableNoun (NounBase "water")


nameJoe : Noun
nameJoe =
    ProperNoun (NounBase "Joe")


theJoes : Noun
theJoes =
    ProperPluralNoun (NounBase "the Joes")


dogSubject : Subject
dogSubject =
    dog |> NounSubject


waterSubject : Subject
waterSubject =
    water |> NounSubject


joeSubject : Subject
joeSubject =
    nameJoe |> NounSubject


firstPersonSubject : Subject
firstPersonSubject =
    I |> PronounSubject


dogObject : Object
dogObject =
    dog |> NounObject


waterObject : Object
waterObject =
    water |> NounObject


joeObject : Object
joeObject =
    nameJoe |> NounObject


firstPersonObject : Object
firstPersonObject =
    I |> PronounObject


testFormatNouns : Test
testFormatNouns =
    describe "UnformattedParagraph"
        [ describe "unchanging subjects and objects"
            [ test "empty paragraph" <|
                \_ ->
                    UnformattedParagraph []
                        |> formatNouns
                        |> paragraphToString
                        |> Expect.equal ""
            , test "only pronouns" <|
                \_ ->
                    UnformattedParagraph
                        [ SimplePresent firstPersonSubject (predicateWithSee firstPersonObject) Period
                        ]
                        |> formatNouns
                        |> paragraphToString
                        |> Expect.equal "I see me."
            , test "proper nouns" <|
                \_ ->
                    UnformattedParagraph
                        [ SimplePresent joeSubject (predicateWithSee joeObject) Period
                        ]
                        |> formatNouns
                        |> paragraphToString
                        |> Expect.equal "Joe sees Joe."
            , test "all sentence types" <|
                \_ ->
                    UnformattedParagraph
                        [ SimplePresent joeSubject (predicateWithSee joeObject) Period
                        , NegativeSimplePresent joeSubject (predicateWithSee joeObject) Period
                        , SimplePast joeSubject (predicateWithSee joeObject) Period
                        , NegativeSimplePast joeSubject (predicateWithSee joeObject) Period
                        ]
                        |> formatNouns
                        |> paragraphToString
                        |> Expect.equal "Joe sees Joe. Joe doesn't see Joe. Joe saw Joe. Joe didn't see Joe."
            ]
        , describe "changing subject"
            [ test "raw countable to indefinite" <|
                \_ ->
                    UnformattedParagraph
                        [ SimplePresent dogSubject (predicateWithSee joeObject) Period
                        ]
                        |> formatNouns
                        |> paragraphToString
                        |> Expect.equal "A dog sees Joe."
            , test "all sentence types" <|
                \_ ->
                    [ [ SimplePresent dogSubject (predicateWithSee joeObject) Period ]
                    , [ NegativeSimplePresent dogSubject (predicateWithSee joeObject) Period ]
                    , [ SimplePast dogSubject (predicateWithSee joeObject) Period ]
                    , [ NegativeSimplePast dogSubject (predicateWithSee joeObject) Period ]
                    ]
                        |> map UnformattedParagraph
                        |> map formatNouns
                        |> map paragraphToString
                        |> Expect.equal
                            [ "A dog sees Joe."
                            , "A dog doesn't see Joe."
                            , "A dog saw Joe."
                            , "A dog didn't see Joe."
                            ]
            ]
        , describe "changing object"
            [ test "raw countable to indefinite single object" <|
                \_ ->
                    UnformattedParagraph
                        [ SimplePresent joeSubject (predicateWithSee dogObject) Period
                        ]
                        |> formatNouns
                        |> paragraphToString
                        |> Expect.equal "Joe sees a dog."
            , test "all sentence types single object" <|
                \_ ->
                    [ [ SimplePresent joeSubject (predicateWithSee dogObject) Period ]
                    , [ NegativeSimplePresent joeSubject (predicateWithSee dogObject) Period ]
                    , [ SimplePast joeSubject (predicateWithSee dogObject) Period ]
                    , [ NegativeSimplePast joeSubject (predicateWithSee dogObject) Period ]
                    ]
                        |> map UnformattedParagraph
                        |> map formatNouns
                        |> map paragraphToString
                        |> Expect.equal
                            [ "Joe sees a dog."
                            , "Joe doesn't see a dog."
                            , "Joe saw a dog."
                            , "Joe didn't see a dog."
                            ]
            , test "raw countable to indefinite first object" <|
                \_ ->
                    UnformattedParagraph
                        [ SimplePresent joeSubject (predicateWithGive dogObject waterObject) Period
                        ]
                        |> formatNouns
                        |> paragraphToString
                        |> Expect.equal "Joe gives a dog water."
            , test "raw countable to indefinite second object" <|
                \_ ->
                    UnformattedParagraph
                        [ SimplePresent joeSubject (predicateWithGive waterObject dogObject) Period
                        ]
                        |> formatNouns
                        |> paragraphToString
                        |> Expect.equal "Joe gives water a dog."
            ]
        , describe "repeated subject is definite"
            [ test "countable subject" <|
                \_ ->
                    UnformattedParagraph
                        [ SimplePresent dogSubject (predicateWithSee joeObject) Period
                        , SimplePresent dogSubject (predicateWithSee firstPersonObject) Period
                        ]
                        |> formatNouns
                        |> paragraphToString
                        |> Expect.equal "A dog sees Joe. The dog sees me."
            , test "uncountable subject" <|
                \_ ->
                    UnformattedParagraph
                        [ SimplePresent waterSubject (predicateWithSee joeObject) Period
                        , SimplePresent waterSubject (predicateWithSee firstPersonObject) Period
                        ]
                        |> formatNouns
                        |> paragraphToString
                        |> Expect.equal "Water sees Joe. The water sees me."
            , test "plural subject" <|
                \_ ->
                    UnformattedParagraph
                        [ SimplePresent (dog |> toPlural |> NounSubject) (predicateWithSee joeObject) Period
                        , SimplePresent (dog |> toPlural |> NounSubject) (predicateWithSee firstPersonObject) Period
                        ]
                        |> formatNouns
                        |> paragraphToString
                        |> Expect.equal "Dogs see Joe. The dogs see me."
            , test "non changing subjects" <|
                \_ ->
                    UnformattedParagraph
                        [ SimplePresent firstPersonSubject (predicateWithSee joeObject) Period
                        , SimplePresent firstPersonSubject (predicateWithSee firstPersonObject) Period
                        , SimplePresent joeSubject (predicateWithSee joeObject) Period
                        , SimplePresent joeSubject (predicateWithSee firstPersonObject) Period
                        , SimplePresent (theJoes |> NounSubject) (predicateWithSee joeObject) Period
                        , SimplePresent (theJoes |> NounSubject) (predicateWithSee firstPersonObject) Period
                        ]
                        |> formatNouns
                        |> paragraphToString
                        |> Expect.equal
                            "I see Joe. I see me. Joe sees Joe. Joe sees me. The Joes see Joe. The Joes see me."
            ]
        ]
