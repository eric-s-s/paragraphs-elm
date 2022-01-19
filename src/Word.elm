module Word exposing (..)


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

type Capitalized = Capital | Lowercase

type Pronoun
    = I
    | Me
    | You
    | He
    | Him
    | She
    | Her
    | We
    | Us
    | They
    | Them

type Word = 
  Pronoun Pronoun Capitalized

toValue : Pronoun -> String
toValue value = 
  case value of
    I -> "I"
    Me -> "Me"
    You -> "You"
    He -> "He"
    Him -> "Him"
    She -> "She"
    Her -> "Her"
    We -> "We"
    Us -> "Us"
    They -> "They"
    Them -> "Them"