{-# LANGUAGE DeriveDataTypeable #-}
module Language.DeckBuild.Syntax where
import Language.Haskell.TH (Pat, Exp, Strict)
import Data.Generics (Data, Typeable)

------------------------------------------------------------------------------
-- Data types and type aliases for a simple deck-building game (Dominion-like)

data CardType   = TREASURE | ACTION  | VICTORY
    deriving (Eq, Data, Typeable, Show)

data EffectType = MONEY    | ACTIONS | BUYS
    deriving (Eq, Data, Typeable, Show)

type CardID     = String

data Card = Card CardID CardType CardDescr CardCost
    deriving (Eq, Data, Typeable, Show)

data CardDescr = CardDescr
  { primary :: [ Effect ]
  , other   ::   String
  } deriving (Eq, Data, Typeable, Show)

data Effect = Effect
  { amount     :: Integer
  , effectType :: EffectType
  } deriving (Eq, Data, Typeable, Show)

type CardCost = Integer

type TurnID = String

data Turn = Turn TurnID [Phase]
  deriving (Eq, Data, Typeable, Show)

data Phase = Phase PhaseName PhaseInt
    deriving (Eq, Data, Typeable, Show)

data PhaseInt = PhaseInt Integer | All
	deriving (Eq, Data, Typeable, Show)

data PhaseName = ActionP  | BuyP  | DiscardP | DrawP 
 	deriving (Eq, Data, Typeable, Show)

-- Do we need this? -- RLV
-- qName :: QString -> String
-- qName [n] = n
-- qName (n:ms) = n ++ "." ++ qName ms
