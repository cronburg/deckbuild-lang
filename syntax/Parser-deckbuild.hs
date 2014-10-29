module DeckBuildParser where
import Text.ParserCombinators.Parsec
import qualified Text.Parsec.String as PS
import qualified Text.Parsec.Prim   as PP
import qualified Text.Parsec.Token  as PT
import Text.ParserCombinators.Parsec.Language (haskellStyle, reservedOpNames, reservedNames)
import Data.Char        -- Provides isDigit and isSpace functions

type Parser = PS.Parser

-- Keywords (KW == Keyword):
{-cardKW     = string "card"
actionKW   = string "action"
costsKW    = string "costs"
treasureKW = string "Treasure"
actionKW   = string "Action"
victoryKW  = string "Victory"
-}

data CardType   = TREASURE | ACTION  | VICTORY
data EffectType = MONEY    | ACTIONS | BUYS
type CardID     = String

data Card = Card CardID CardType CardDescr CardCost
data CardDescr = CardDescr UpperDescr LowerDescr
type UpperDescr = String
type LowerDescr = [Effect]
type Effect = (Integer,EffectType)
type CardCost = Integer

(<||>) a b = try a <|> try b

-- A card definition file has 0 or more card declarations:
cardFile = many cardDecl

-- A card declaration:
cardDecl = do
  reserved "card"; cID <- cardID;
  reserved "::";   cTY <- cardType;
  descr <- braces cardDescr;
  reserved "costs"; cost <- integer;
  return $ Card cID cTY descr cost

cType s = do
  reserved s;
  return (case s of
    "Treasure" -> TREASURE
    "Action"   -> ACTION
    "Victory"  -> VICTORY)

cardType = cType "Treasure" <||> cType "Action" <||> cType "Victory"

cardID = identifier

cardDescr = do
  d1 <- upperDescr;
  d2 <- many effectDescr;
  return $ CardDescr d1 d2

eType s = do
  reserved s;
  return (case s of
    "actions" -> ACTIONS
    "coins"   -> MONEY
    "buys"    -> BUYS)

signValue :: String -> Integer
signValue s = do
  reserved s;
  return (case s of
    "+" -> 1
    "-" -> -1)

upperDescr = stringLiteral
effectDescr = do
  sign   <- (signValue "+" <||> signValue "-");
  i      <- integer;
  effect <- (eType "actions" <||> eType "coins" <||> eType "buys")
  return $ ((*) sign i, effect)

------------------------------------------------------------------------------
-- Lexer
lexer :: PT.TokenParser ()
lexer = PT.makeTokenParser (haskellStyle 
  { reservedOpNames = ["::", "{", "}", "+", "-"],
    reservedNames   = ["Treasure", "costs", "card", "action", "coins", "buys",
                       "Victory"]
  })

--"=", "=>", "{", "}", "::", "<|", "|>", "|", reMark, "." ],
--"data", "type", "newtype", "old", "existing", "deriving",
--                       "using", "where", "terminator", "length", "of", "from",
--                       "case", "constrain", "obtain", "partition","value" ]}) 

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

