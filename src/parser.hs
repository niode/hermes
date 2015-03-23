module Parser
  (ParsedThing) where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

data ParsedThing = PlaceHolder String

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
 -        | Article x
 -
 -  Verb := x
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

data Statement = Stmt Action deriving Show
data Action = AVerb Verb
            | ATarget Verb Preposition Noun
            | AConjunction Action Action
            deriving Show
data Preposition = PIn | POf | PTo | POn deriving Show
data Conjunction = Conj deriving Show
data Article = The | A | An deriving Show
data Noun = NounConst String
          | Article String
          deriving Show
data Verb = VerbConst String
          | Use Noun
          | UseTarget Noun Preposition Noun
          | Do Noun Preposition Noun
          | Apply Noun Preposition Noun
          deriving Show
data AdHoc = AdHoc String deriving Show

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
                                    ]
           }

lexer = Token.makeTokenParser languageDef

identifier  = Token.identifier  lexer
reserved    = Token.reserved    lexer
integer     = Token.integer     lexer
whitespace  = Token.whiteSpace  lexer

-- Cleans up initial whitespace (parser only handles whitespace after tokens).
--clean :: Parser Statement
--clean = whitespace >> statement

--statement :: Parser Statement
--statement = verbStmt <|> verbPrepStmt <|> actionStmt
