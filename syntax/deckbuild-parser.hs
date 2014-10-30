module DeckBuildParser where
import Text.ParserCombinators.Parsec
import qualified Text.Parsec.String as PS
import qualified Text.Parsec.Prim   as PP
import qualified Text.Parsec.Token  as PT
import Text.ParserCombinators.Parsec.Language (haskellStyle, reservedOpNames, reservedNames)
import Data.Char        -- Provides isDigit and isSpace functions

import Language.DeckBuild.Syntax

type Parser = PS.Parser

------------------------------------------------------------------------------
-- Or-Try Combinator (tries two parsers, one after the other)
(<||>) a b = try a <|> try b

------------------------------------------------------------------------------
-- Parser functions for our language

-- A card definition file has 0 or more card declarations:
cardFile = many cardDecl

-- A card declaration:
cardDecl = do
  { reserved "card"
  ; cID <- cardID
  ; reserved "::"
  ; cTY <- cardType
  ; descr <- braces cardDescr
  ; reserved "costs"
  ; cost <- integer
  ; return $ Card cID cTY descr cost }

-- Attempts to parse the given reserved string card type keyword,
-- returning the corresponding CardType
cType s = do
  reserved s;
  return (case s of
    "Treasure" -> TREASURE
    "Action"   -> ACTION
    "Victory"  -> VICTORY)

-- Tries to parse different card types one-by-one
cardType = cType "Treasure" <||> cType "Action" <||> cType "Victory"

-- The name (ID) of a card is just a regular identifier
cardID = identifier

-- Parse the description on a card
cardDescr = do
  { d1 <- many effectDescr
  ; d2 <- englishDescr
  ; return $ CardDescr { primary = d1, other = d2 } }

-- Attempts to parse the given reserved string effect keyword,
-- returning the corresponding EffectType
eType s = do
  { reserved s
  ; return (case s of
              "actions" -> ACTIONS
              "coins"   -> MONEY
              "buys"    -> BUYS) }

-- Returns (+1) if "+" is parsed, or (-1) if "-" is parsed
signValue s = do
  { reserved s
  ; return (case s of
            "+" -> 1
            "-" -> -1) }

-- Lower-half description of a card (non-bold-text), is just a literal
-- string for now (presumably in English)
englishDescr = stringLiteral

-- Parses effect (upper-half) description of a card (bold-face-text)
effectDescr = do
  { sign   <- (signValue "+" <||> signValue "-")
  ; i      <- integer
  ; effect <- (eType "actions" <||> eType "coins" <||> eType "buys")
  ; return $ Effect { amount = sign * i, effectType = effect } }

------------------------------------------------------------------------------
-- Lexer
lexer :: PT.TokenParser ()
lexer = PT.makeTokenParser (haskellStyle 
  { reservedOpNames = ["::", "{", "}", "+", "-"],
    reservedNames   = ["Treasure", "costs", "card", "action", "coins", "buys",
                       "Victory"]
  })

whiteSpace    = PT.whiteSpace  lexer
identifier    = PT.identifier  lexer
operator      = PT.operator    lexer
reserved      = PT.reserved    lexer
reservedOp    = PT.reservedOp  lexer
charLiteral   = PT.charLiteral lexer
stringLiteral = PT.stringLiteral  lexer
integer       = PT.integer     lexer
commaSep1     = PT.commaSep1   lexer
parens        = PT.parens      lexer
braces        = PT.braces      lexer
brackets      = PT.brackets    lexer

