module Word exposing (..)


type Capitalized = Capital | Lowercase

type BasePronoun
    = I
    | Me
    | You
    | He
    | Him
    | She
    | Her
    | It
    | We
    | Us
    | They
    | Them

toValue : BasePronoun -> String
toValue value = 
  case value of
    I -> "I"
    Me -> "Me"
    You -> "You"
    He -> "He"
    Him -> "Him"
    She -> "She"
    Her -> "Her"
    It -> "It"
    We -> "We"
    Us -> "Us"
    They -> "They"
    Them -> "Them"

type Word = 
  Pronoun BasePronoun Capitalized

toString : Word -> String
toString value = 
  case value of
    Pronoun base Capital -> toValue base
    Pronoun I Lowercase -> "I"
    Pronoun base Lowercase -> String.toLower <| toValue base

