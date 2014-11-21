{-# LANGUAGE DeriveDataTypeable #-}
module Language.DeckBuild.Syntax where
import Language.Haskell.TH (Pat, Exp, Strict)
import Language.Haskell.TH.Syntax (lift, mkName, Exp( ConE ), Lift, Exp( ListE ), Exp( LitE ), Exp( RecConE ))
import Data.Generics (Data, Typeable)

------------------------------------------------------------------------------
-- Data types and type aliases for a simple deck-building game (Dominion-like)

data DeckDecl = DeckDeclCard  Card
              | DeckDeclTurn  Turn
   deriving (Eq, Data, Typeable, Show)

-- Lift option:
liftD (DeckDeclCard card) = return $ liftCard card
liftD (DeckDeclTurn turn) = undefined -- return $ liftD turn

liftCard (Card { cID    = cardid
               , cType  = cardType
               , cDescr = cardDescr
               , cCost  = cardCost }
               ) = do
    lcid <- lift cardid
    lctype <- liftCtype cardType
    lcdescr <- liftCdescr cardDescr
    lccost <- lift cardCost
    return $ RecConE (mkName "Card") [ (mkName "cID", lcid)
                                     , (mkName "cType", lctype)
                                     , (mkName "cDescr", lcdescr)
                                     , (mkName "cCost", lccost) ]

liftCtype TREASURE = return $ ConE (mkName "TREASURE")
liftCtype ACTION = return $ ConE (mkName "ACTION")
liftCtype VICTORY = return $ ConE (mkName "VICTORY")

liftCdescr (CardDescr { primary = effects
                      , other = otherEffect }
                      ) = do
    es <- mapM liftEffect effects
    o <- lift otherEffect
    return $ RecConE (mkName "CardDescr") [ (mkName "primary", ListE es)
                                          , (mkName "other", o) ]

liftEffect (Effect { amount = amt
                   , effectType = etype }
                   ) = do
    a <- lift amt
    e <- liftEtype etype
    return $ RecConE (mkName "Effect") [ (mkName "amount", a)
                                       , (mkName "effectType", e) ]

liftEtype COINS = return conCoins
liftEtype BUYS = return conBuys
liftEtype ACTIONS = return conActions
liftEtype CARDS = return conCards
liftEtype VICTORYPOINTS = return conVictoryPoints

conCoins = ConE (mkName "COINS")
conActions = ConE (mkName "ACTIONS")
conBuys = ConE (mkName "BUYS")
conCards = ConE (mkName "CARDS")
conVictoryPoints = ConE (mkName "VICTORYPOINTS")

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
