{-# LANGUAGE DeriveDataTypeable #-}
module Language.DeckBuild.Syntax where
import Language.Haskell.TH (Pat, Exp, Strict)
import Language.Haskell.TH.Syntax (lift, mkName, Exp( ConE ), Lift, Exp( ListE ), Exp( LitE ), Exp( RecConE ), Exp( AppE ))
import Data.Generics (Data, Typeable)
import Data.Char (toUpper)

------------------------------------------------------------------------------
-- Data types and type aliases for a simple deck-building game (Dominion-like)

data DeckDecl = DeckDeclCard  Card
              | DeckDeclTurn  Turn
   deriving (Eq, Typeable, Show)

-- Lift option:
liftD (DeckDeclCard card) = liftCard card
liftD (DeckDeclTurn turn) = liftTurn turn

-- Card lift functions
liftCard (Card { cID    = cardid
               , cType  = cardType
               , cDescr = cardDescr
               , cCost  = cardCost }
               ) = do
    --lcid <- lift cardid
    let lcid = ConE $ mkName $ map toUpper cardid
    lctype <- liftCtype cardType
    lcdescr <- liftCdescr cardDescr
    lccost <- lift cardCost
    return $ RecConE (mkName "RuntimeCard") [ (mkName "cID", lcid)
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

-- Card lift functions
liftTurn (Turn { turnID    = turnid
               , turnPhases = phases }
               ) = do
    ltid <- lift turnid
    ps <- mapM liftPhase phases 
    return $ RecConE (mkName "Turn") [ (mkName "turnID", ltid)
                                     , (mkName "turnPhases", ListE ps) ]

liftPhase (Phase { phaseName = pname
                 , phaseInt = pint }
                 ) = do
    a <- liftPhaseName pname
    p <- liftPhaseInt pint
    return $ RecConE (mkName "Phase") [ (mkName "phaseName", a)
                                      , (mkName "phaseInt", p) ]

liftPhaseInt (PhaseInt phase) = do
    p <- lift phase
    return $ AppE (ConE $ mkName "PhaseInt") p 
liftPhaseInt All = return $ ConE (mkName "All")

liftPhaseName ActionP = return $ ConE (mkName "ActionP")
liftPhaseName BuyP = return $ ConE (mkName "BuyP")
liftPhaseName DiscardP = return $ ConE (mkName "DiscardP")
liftPhaseName DrawP = return $ ConE (mkName "DrawP")

-- Data declarations
data CardType   = TREASURE | ACTION  | VICTORY
    deriving (Eq, Typeable, Show)

data EffectType = COINS    | ACTIONS | BUYS | CARDS | VICTORYPOINTS
    deriving (Eq, Typeable, Show)

type CardID     = String

data Card = Card
  { cID    :: CardID
  , cType  :: CardType
  , cDescr :: CardDescr
  , cCost  :: CardCost
  } deriving (Eq, Typeable, Show)

data CardDescr = CardDescr
  { primary :: [ Effect ]
  , other   ::   String
  } deriving (Eq, Typeable, Show)

data Effect = Effect
  { amount     :: Int
  , effectType :: EffectType
  } deriving (Eq, Typeable, Show)

type CardCost = Int

type TurnID = String

data Turn = Turn
  { turnID     :: TurnID
  , turnPhases :: [Phase]
  } deriving (Eq, Typeable, Show)

data Phase = Phase
  { phaseName :: PhaseName
  , phaseInt  :: PhaseInt
  } deriving (Eq, Typeable, Show)

data PhaseInt = PhaseInt Int | All
	deriving (Eq, Typeable, Show)

data PhaseName = ActionP  | BuyP  | DiscardP | DrawP
 	deriving (Eq, Typeable, Show)

-- Do we need this? -- RLV
-- qName :: QString -> String
-- qName [n] = n
-- qName (n:ms) = n ++ "." ++ qName ms
