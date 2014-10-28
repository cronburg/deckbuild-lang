module PCParser where
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

data CardType   = TREASURE | ACTION | VICTORY
data EffectType = 

data Card = Card CardType CardDescr CardCost
data CardDescr = CardDescr UpperDescr LowerDescr
type UpperDescr = String
data LowerDescr = [Effect]
type Effect = (Integer,
type CardCost = Integer

(<||>) a b = try a <|> try b

-- A card definition file has 0 or more card declarations:
cardFile = many cardDecl

-- A card declaration:
cardDecl = reserved "card" >> cardID >> reserved "::" >> cardType >>
           braces cardDescr >> reserved "costs" >> integer

cardType = reserved "Treasure" <|> reserved "Action" <|> reserved "Victory"

cardID = identifier

cardDescr = upperDescr >> many lowerDescr
upperDescr = stringLiteral
lowerDescr = (reserved "+" <||> reserved "-") >> integer >>
             (reserved "actions" <||> reserved "coins" <||>
              reserved "buys" <||> reserved "victory")

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

