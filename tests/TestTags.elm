module TestTags exposing (..)

import Expect
import Test exposing (..)
import Tags exposing (emptyTags)
import Tags exposing (..)
import Set

suite = 
    describe "Testing tags" 
    [
        test "empty tags" <|
            (\_ ->
                emptyTags |> Expect.equal (Tags Set.empty)
            )
        , test "tags from list" <|
            (\_ ->
                toTags [Tags.Proper, Tags.Uncountable] 
                |> Expect.equal
                   (Tags (Set.fromList [
                            (tagToInt Tags.Proper)
                            , (tagToInt Tags.Uncountable)
                        ]))
            )
        , test "add tag empty" <|
            (\_ -> 
                addTag emptyTags Tags.ThirdPerson
                |> Expect.equal (toTags [Tags.ThirdPerson])
            )
        , test "add tag already present" <|
            (\_ -> 
                addTag (toTags [Tags.ThirdPerson]) Tags.ThirdPerson
                |> Expect.equal (toTags [Tags.ThirdPerson])
            )
        , test "remove tag" <|
            (\_ -> 
                removeTag (toTags [Tags.ThirdPerson]) Tags.ThirdPerson
                |> Expect.equal (emptyTags)
            )
        , test "remove tag not present" <|
            (\_ -> 
                removeTag (toTags [Tags.ThirdPerson]) Tags.Indefinite
                |> Expect.equal (toTags [ThirdPerson])
            )
        , test "has tag true" <|
            (\_ ->
                Tags.Uncountable
                |> hasTag (toTags [Tags.ThirdPerson, Tags.Uncountable])
                |> Expect.equal True
            )
        , test "has tag false" <|
            (\_ ->
                hasTag (toTags [Tags.ThirdPerson]) Tags.Uncountable
                |> Expect.equal False
            )
    ]