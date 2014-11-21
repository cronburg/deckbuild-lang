module Language.DeckBuild.Parser
  ( cardFile, turnDecl, ruleFile, cardDecl, deckDecls, parseDeckDecls )
  where

import Text.ParserCombinators.Parsec
import qualified Text.Parsec.String as PS
import qualified Text.Parsec.Prim   as PP
import qualified Text.Parsec.Token  as PT
import qualified Text.Parsec.Expr as PE
import Text.ParserCombinators.Parsec.Language (haskellStyle, reservedOpNames, reservedNames)
import Text.ParserCombinators.Parsec.Pos      (newPos)
import Data.Char        -- Provides isDigit and isSpace functions

import Language.DeckBuild.Syntax hiding (turnID, phaseName)

type Parser = PS.Parser

parseDeckDecls :: SourceName -> Line -> Column -> String -> Either ParseError [DeckDecl]
parseDeckDecls fileName line column input
  = PP.parse (do { setPosition (newPos fileName line column)
                 ; whiteSpace
                 ; x <- deckDecls
                 ; whiteSpace
                 ; eof <|> errorParse
                 ; return x
                 }) fileName input

errorParse = do
  { rest <- manyTill anyToken eof
  ; unexpected rest }

------------------------------------------------------------------------------
-- Top-level parser:

deckDecls = many deckDecl
deckDecl  = deckDeclCard <||> deckDeclTurn

deckDeclCard = cardDecl >>= \c -> return $ DeckDeclCard c
deckDeclTurn = turnDecl >>= \t -> return $ DeckDeclTurn t

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
  ; return $ Card cID cTY descr cost
  }

-- Attempts to parse the given reserved string card type keyword,
-- returning the corresponding CardType
cardType' s = do
  { reserved s
  ; return $ case s of
      "Treasure" -> TREASURE
      "Action"   -> ACTION
      "Victory"  -> VICTORY
  }

-- Tries to parse different card types one-by-one
cardType = cardType' "Treasure" <||> cardType' "Action" <||> cardType' "Victory"

-- The name (ID) of a card is just a regular identifier
cardID = identifier

-- Parse the description on a card
cardDescr = do
  {
  ; d1 <- many effectDescr
  ; d2 <- englishDescr
  ; return $ CardDescr { primary = d1, other = concat d2 }
  }

-- Attempts to parse the given reserved string effect keyword,
-- returning the corresponding EffectType
eType s = do
  { reserved s
  ; return $ case s of
              "actions" -> ACTIONS
              "coins"   -> COINS
              "buys"    -> BUYS
  }


-- Lower-half description of a card (non-bold-text), is just a literal
-- string for now (presumably in English)
englishDescr = many stringLiteral

-- Parses effect (upper-half) description of a card (bold-face-text)
effectDescr = do
  { PP.lookAhead (char '+' <||> char '-')
  ; amount <- expr
  ; effect <- (eType "actions" <||> eType "coins" <||> eType "buys")
  ; return $ Effect { amount = amount, effectType = effect }
  }
---------------
-- Custom Rules Parsing

-- placeholder
ruleFile = many turnDecl

-- returns a Turn'

turnDecl = do
  { reserved "turn"
  ; tID <- turnID
  ; phases <- braces (many phaseDescr)
  ; return $ Turn tID phases
  }

turnID = identifier

phaseDescr = do
  { phase <- phaseName
  ; amount <- phaseAmount
  ; return $ Phase phase amount
  }

phaseAmount = (reserved "all" >>        return All)
         <||> (integer        >>= \i -> return $ PhaseInt i)

phaseName = phaseType "action" <||> phaseType "buy" <||> phaseType "discard" <||> phaseType "draw"

phaseType s = do
  { reserved s
  ; return $ case s of
              "action"  -> ActionP
              "buy"     -> BuyP
              "discard" -> DiscardP
              "draw"    -> DrawP
  }

------------------------------------------------------------------------------
-- Lexer
lexer :: PT.TokenParser ()
lexer = PT.makeTokenParser $ haskellStyle
  { reservedOpNames = ["::", "{", "}", "+", "-"]
  , reservedNames   = ["Treasure", "costs", "card", "action", "coins", "buys",
                       "Victory","turn","all","buy","discard","draw"]
  }

whiteSpace    = PT.whiteSpace  lexer
identifier    = PT.identifier  lexer
operator      = PT.operator    lexer
reserved      = PT.reserved    lexer
reservedOp    = PT.reservedOp  lexer
charLiteral   = PT.charLiteral lexer
stringLiteral = PT.stringLiteral  lexer
integer       = PT.integer     lexer
natural       = PT.natural     lexer
commaSep1     = PT.commaSep1   lexer
parens        = PT.parens      lexer
braces        = PT.braces      lexer
brackets      = PT.brackets    lexer

expr = PE.buildExpressionParser table term
       <?> "expression"
term = natural
       <?> "simple expression"
table = [ [prefix "-" negate, prefix "+" id ] ]
prefix   name fun = PE.Prefix $ reservedOp name >> return fun
