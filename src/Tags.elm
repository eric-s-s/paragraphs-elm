module Tags exposing (..)
import Set exposing (Set)
type WordTag
  = Proper
  | Uncountable
  | Definite
  | Indefinite
  | Plural

  | ThirdPerson
  | Negative
  | Past

  | Preposition
  | SeparableParticle


type Tags = Tags (Set Int)

tagToInt : WordTag -> Int
tagToInt tag = 
    case tag of
        Proper -> 1
        Uncountable -> 2
        Definite -> 3
        Indefinite -> 4
        Plural -> 5

        ThirdPerson -> 6
        Negative -> 7
        Past -> 8

        Preposition -> 9
        SeparableParticle -> 10

hasTag : Tags -> WordTag -> Bool
hasTag (Tags tags) tag = Set.member (tagToInt tag) tags

addTag : Tags -> WordTag -> Tags
addTag (Tags tags) newTag = Tags <| Set.insert (tagToInt newTag ) tags

removeTag : Tags -> WordTag -> Tags
removeTag (Tags tags) newTag = Tags <| Set.remove (tagToInt newTag ) tags

emptyTags : Tags
emptyTags = Tags Set.empty

toTags: List WordTag -> Tags
toTags tagList = Tags <| Set.fromList <| List.map tagToInt tagList