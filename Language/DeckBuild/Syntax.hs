{-# LANGUAGE DeriveDataTypeable #-}
module Language.DeckBuild.Syntax where
import Language.Haskell.TH (Pat, Exp, Strict)
import Language.Haskell.TH.Syntax (Lift)
import Data.Generics (Data, Typeable)

------------------------------------------------------------------------------
-- Data types and type aliases for a simple deck-building game (Dominion-like)

data DeckDecl = DeckDeclCard  Card
              | DeckDeclTurn  Turn
   deriving (Eq, Data, Typeable, Show)

-- Lift option:
instance Lift DeckDecl where
liftD (DeckDeclCard card) = return $ liftCard card
liftD (DeckDeclTurn turn) = undefined -- return $ liftD turn

liftCard (Card { cID    = cardID
               , cType  = cardType
               , cDescr = cardDescr
               , cCost  = cardCost }
               ) = return $ RecConE (mkName "Card") [ (mkName "cID", lift cardID)
                                                    , (mkName "cType", liftCtype cardType)
                                                    , (mkName "cDescr", lift cardDescr
                                                    , (mkName "cCost", lift cardCost) ]

liftCtype TREASURE = ConE (mkName "TREASURE")
liftCtype ACTION = ConE (mkName "ACTION")
liftCtype VICTORY = ConE (mkName "VICTORY")

data CardType   = TREASURE | ACTION  | VICTORY
    deriving (Eq, Data, Typeable, Show)

data EffectType = COINS    | ACTIONS | BUYS | CARDS | VICTORYPOINTS
    deriving (Eq, Data, Typeable, Show)

type CardID     = String

data Card = Card
  { cID    :: CardID
  , cType  :: CardType
  , cDescr :: CardDescr
  , cCost  :: CardCost
  } deriving (Eq, Data, Typeable, Show)

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

data Turn = Turn
  { turnID     :: TurnID
  , turnPhases :: [Phase]
  } deriving (Eq, Data, Typeable, Show)

data Phase = Phase
  { phaseName :: PhaseName
  , phaseInt  :: PhaseInt
  } deriving (Eq, Data, Typeable, Show)

data PhaseInt = PhaseInt Integer | All
	deriving (Eq, Data, Typeable, Show)

data PhaseName = ActionP  | BuyP  | DiscardP | DrawP
 	deriving (Eq, Data, Typeable, Show)

-- Do we need this? -- RLV
-- qName :: QString -> String
-- qName [n] = n
-- qName (n:ms) = n ++ "." ++ qName ms
