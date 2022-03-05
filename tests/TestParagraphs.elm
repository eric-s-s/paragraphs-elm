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
        )
import WordData exposing (NumberOfObjects(..), VerbData)


dog : Noun
dog =
    RawCountableNoun (NounBase "dog") NoIrregularPlural


water : Noun
water =
    UncountableNoun (NounBase "water")


nameJoe : Noun
nameJoe =
    ProperNoun (NounBase "Joe")


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
                    [ [SimplePresent dogSubject (predicateWithSee joeObject) Period]
                    , [NegativeSimplePresent dogSubject (predicateWithSee joeObject) Period]
                    , [SimplePast dogSubject (predicateWithSee joeObject) Period]
                    , [NegativeSimplePast dogSubject (predicateWithSee joeObject) Period]
                    ]
                        |> map UnformattedParagraph
                        |> map formatNouns
                        |> map paragraphToString
                        |> Expect.equal [
                            "A dog sees Joe."
                            , "A dog doesn't see Joe."
                            , "A dog saw Joe."
                            , "A dog didn't see Joe."
                        ]
            ]
        ]
