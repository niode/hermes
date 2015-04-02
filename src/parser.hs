module Parser
  ( Action(..)
  , Preposition(..)
  , Conjunction(..)
  , Article(..)
  , Noun(..)
  , Verb(..)
  , parseCommand) where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

{-
 - Grammar:
 -  Statement := Action
 -
 -  Action := Verb | Verb Preposition Noun | Action Conjunction Action
 -
 -  Preposition := in | of | to | on
 -
 -  Conjunction := and | and then
 -
 -  Article := the | a | an
 -
 -  Noun := x
 -        | Integer
 -        | Article x
 -
 -  Verb := x
 -        | x with Noun
 -        | use Noun
 -        | use Noun Preposition Noun
 -        | do Noun Preposition Noun
 -        | apply Noun Preposition Noun
 -
 -  AdHoc := x
 -
 -  Note: "System commands" such as "exit" or "info" will be implemented by
 -  invisible items.
 -
 -  See wiki.haskell.org/Parsing_a_simple_imperative_language
 -}

data Action = AVerb Verb
            | ATarget Verb Preposition Noun
            | AConjunction Action Action
            | AError String
            | ANull
            deriving (Show, Eq)
data Preposition = PIn | POf | PTo | POn deriving (Show, Eq)
data Conjunction = Conj deriving (Show, Eq)
data Article = The | A | An deriving (Show, Eq)
data Noun = NounConst String
          | NounInt Integer
          | ArticleNoun Article String
          deriving (Show, Eq)
data Verb = VerbConst String
          | With String Noun
          | Use Noun
          | UseTarget Noun Preposition Noun
          | Do Noun Preposition Noun
          | Apply Noun Preposition Noun
          deriving (Show, Eq)

languageDef =
  emptyDef {  Token.identStart  = letter
           ,  Token.identLetter = alphaNum
           ,  Token.reservedNames = [ "in"
                                    , "of"
                                    , "to"
                                    , "on"
                                    , "then"
                                    , "and"
                                    , "the"
                                    , "a"
                                    , "an"
                                    , "with"
                                    ]
           }

lexer = Token.makeTokenParser languageDef

identifier  = Token.identifier  lexer
reserved    = Token.reserved    lexer
integer     = Token.integer     lexer
whitespace  = Token.whiteSpace  lexer
parens      = Token.parens      lexer

parseCommand::String->Action
parseCommand [] = ANull
parseCommand s =
  case parse clean "" s of
    Left  e -> AError (show e)
    Right r -> r

-- Cleans up initial whitespace (parser only handles whitespace after tokens).
clean :: Parser Action
clean = whitespace >> action

action :: Parser Action
--action = conjAction <|> verbAction <|> verbNounAction
action = verbAction <|> verbNounAction

verbAction :: Parser Action
verbAction = do
  v <- verb
  return $ AVerb v

verbNounAction :: Parser Action
verbNounAction = do
  v <- verb
  p <- preposition
  n <- noun
  return $ ATarget v p n

conjAction :: Parser Action
conjAction = do
  a1 <- action
  _  <- conjunction
  a2 <- action
  return $ AConjunction a1 a2

verb :: Parser Verb
verb = verbC <|> verbB <|> verbD <|> verbE <|> verbA

verbWith = do
  var <- identifier
  reserved "with"
  n <- noun
  return $ With var n

-- A should go last since it accepts anything.
verbA = do
  var <- identifier
  return $ VerbConst var

verbB = do
  reserved "use"
  n <- noun
  return $ Use n

verbC = do
  reserved "use"
  n1 <- noun
  p  <- preposition
  n2 <- noun
  return $ UseTarget n1 p n2

verbD = do
  reserved "do"
  n1 <- noun
  p  <- preposition
  n2 <- noun
  return $ Do n1 p n2

verbE = do
  reserved "apply"
  n1 <- noun
  p  <- preposition
  n2 <- noun
  return $ Apply n1 p n2

noun :: Parser Noun
noun = nounInt <|> nounB <|> nounA

nounInt = (do p <- parens nounInt; return p)
      <|> (do i <- integer; return $ NounInt i)

nounA = do
  var <- identifier
  return $ NounConst var

nounB = do
  a <- article
  var <- identifier
  return $ ArticleNoun a var

article :: Parser Article
article = articleA <|> articleThe <|> articleAn
articleA = do
  reserved "a"
  return A

articleThe = do
  reserved "the"
  return The

articleAn = do
  reserved "an"
  return An

preposition :: Parser Preposition
preposition = (do reserved "in"; return PIn)
          <|> (do reserved "of"; return POf)
          <|> (do reserved "to"; return PTo)
          <|> (do reserved "on"; return POn)

conjunction :: Parser Conjunction
conjunction = (do reserved "and"; return Conj)
          <|> (do reserved "then"; return Conj)
