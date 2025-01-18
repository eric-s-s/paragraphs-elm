module TextGenerator exposing (..)

module WordData exposing ( lotsaNouns, lotsaVerbs )

getRandomNoun : String
getRandomNoun

getRandomVerb : String
getRandomVerb

generateText : String
generateText = 
                    UnformattedParagraph
                        [ SimplePresent joeSubject (predicateWithSee joeObject) Period
                        , NegativeSimplePresent joeSubject (predicateWithSee joeObject) Period
                        , SimplePast joeSubject (predicateWithSee joeObject) Period
                        , NegativeSimplePast joeSubject (predicateWithSee joeObject) Period
                        ]
                        |> formatNouns
                        |> paragraphToString
