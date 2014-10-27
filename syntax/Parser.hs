module MonadParser where
import Data.Char        -- Provides isDigit and isSpace functions

{- 
   The following imports and instance declarations have to do with 
   new requirements for the Monad typeclass in GHC 7.10.  
-}
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)

instance Functor Parser where
    fmap = liftM
instance Applicative Parser where
    pure  = return
    (<*>) = ap
instance Functor Hopefully where
    fmap = liftM
instance Applicative Hopefully where
    pure  = return
    (<*>) = ap



{- This obviously has to change: - RLV
  In class, we defined the Monad Hopefully for processing errors 
  and the Monad (State t) for threading state with type t through a computation.  
  In this assignment, you will be defining a new monad that does both at the 
  same time in support of parsing strings that match the grammar
  (If this grammar notation is confusing, let me know and I'll explain.)

   Digit ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 
   Int ::= Digit+
   Exp ::= Int | '(' Exp ')' | '+' Exp Exp | '-' Exp Exp
                             | '*' Exp Exp | '/' Exp Exp

   and representing them in a version of the Exp datatype we saw in class:
-}
{-
data Exp = Plus  Exp Exp
         | Minus Exp Exp
         | Times Exp Exp
         | Div   Exp Exp
         | Const Int
      deriving (Show)
-}

data Parser a = P {runParser :: String -> (Hopefully a, String)}
{-
   Note that the above declaration for Parser a creates
   the functions:

      P         :: (String -> (Hopefully a, String)) -> Parser a 
      runParser :: Parser a -> String -> (Hopefully a, String)

   for constructing and deconstructing values of type Parser a, 
   respectively.
-}


{-
   The Hopefully datatype lets us record whether there is an error.
-}
data Hopefully a = Ok a | Error String
    deriving Show

instance Monad Parser where
    return = returnParser
    (>>=)  = bindParser

{- RLV - removed the code from my assignment 
 -}
{- 
   ACTION: Fill in the definiton of returnParser.
   The function should lift a simple value into the Parser monad
   without changing the state. 
-}
returnParser :: a -> Parser a
returnParser a = undefined


{- 
   ACTION: Fill in the definition of bindParser.
   If an error occurs while parsing the first expression,
   that error should be the result of the parse and 
   no further parsing should be attempted.
-}
bindParser :: Parser a -> (a -> Parser b) -> Parser b
bindParser p k = undefined

{- To start parsing our expressions, we'll need a way to parse a ????? - RLV. -}



{-
   I think we need the following. - RLV
-}
instance Monad Hopefully where 
  return = Ok
  (>>=)  = ifOKthen

ifOKthen :: Hopefully a -> (a -> Hopefully b) -> Hopefully b
e `ifOKthen` k = 
   case e of 
     Ok x -> k x
     Error s -> Error s


