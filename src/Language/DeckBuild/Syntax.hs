{-# LANGUAGE DeriveDataTypeable,TemplateHaskell,ScopedTypeVariables,KindSignatures #-}
module Language.DeckBuild.Syntax where
import Language.Haskell.TH (Pat, Exp, Strict, Q)
import Language.Haskell.TH.Syntax (lift, mkName, Exp( ConE ), Lift, Exp( ListE ), Exp( LitE ), Exp( RecConE ), Exp( AppE ))
import Data.Generics (Data, Typeable)
import Data.Char (toUpper)

------------------------------------------------------------------------------
-- Data types and type aliases for a simple deck-building game (Dominion-like)

data DeckDecl = DeckDeclCard  Card
              | DeckDeclTurn  Turn
   deriving (Eq, Typeable, Show, Ord)

-- Lift option:
liftD :: DeckDecl -> Q Exp
liftD (DeckDeclCard card) = liftCard card
liftD (DeckDeclTurn turn) = liftTurn turn

-- Card lift functions
liftCard :: Card -> Q Exp
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

liftCtype :: CardType -> Q Exp
liftCtype TREASURE = return $ ConE (mkName "TREASURE")
liftCtype ACTION = return $ ConE (mkName "ACTION")
liftCtype VICTORY = return $ ConE (mkName "VICTORY")

liftCdescr :: CardDescr -> Q Exp
liftCdescr (CardDescr { primary = effects
                      , other = otherEffect }
                      ) = do
    es <- mapM liftEffect effects
    o <- lift otherEffect
    return $ RecConE (mkName "CardDescr") [ (mkName "primary", ListE es)
                                          , (mkName "other", o) ]

liftEffect :: Effect -> Q Exp
liftEffect (Effect { amount = amt
                   , effectType = etype }
                   ) = do
    a <- lift amt
    e <- liftEtype etype
    return $ RecConE (mkName "Effect") [ (mkName "amount", a)
                                       , (mkName "effectType", e) ]

liftEtype :: EffectType -> Q Exp
liftEtype COINS = return $ ConE (mkName "COINS")
liftEtype BUYS = return $ ConE (mkName "BUYS")
liftEtype ACTIONS = return $ ConE (mkName "ACTIONS")
liftEtype CARDS = return $ ConE (mkName "CARDS")
liftEtype VICTORYPOINTS = return $ ConE (mkName "VICTORYPOINTS")


-- Card lift functions
liftTurn :: Turn -> Q Exp
liftTurn (Turn { turnID    = turnid
               , turnPhases = phases }
               ) = do
    ltid <- lift turnid
    ps <- mapM liftPhase phases 
    return $ RecConE (mkName "Turn") [ (mkName "turnID", ltid)
                                     , (mkName "turnPhases", ListE ps) ]

liftPhase :: Phase -> Q Exp
liftPhase (Phase { phaseName = pname
                 , phaseInt = pint }
                 ) = do
    a <- liftPhaseName pname
    p <- liftPhaseInt pint
    return $ RecConE (mkName "Phase") [ (mkName "phaseName", a)
                                      , (mkName "phaseInt", p) ]

liftPhaseInt :: PhaseInt -> Q Exp
liftPhaseInt (PhaseInt phase) = do
    p <- lift phase
    return $ AppE (ConE $ mkName "PhaseInt") p 
liftPhaseInt All = return $ ConE (mkName "All")

liftPhaseName :: PhaseName -> Q Exp
liftPhaseName ActionP = return $ ConE (mkName "ActionP")
liftPhaseName BuyP = return $ ConE (mkName "BuyP")
liftPhaseName DiscardP = return $ ConE (mkName "DiscardP")
liftPhaseName DrawP = return $ ConE (mkName "DrawP")

-- Data declarations
data CardType   = TREASURE | ACTION  | VICTORY
    deriving (Eq, Typeable, Show, Ord)

data EffectType = COINS    | ACTIONS | BUYS | CARDS | VICTORYPOINTS
    deriving (Eq, Typeable, Show, Ord)

type CardID     = String

data Card = Card
  { cID    :: CardID
  , cType  :: CardType
  , cDescr :: CardDescr
  , cCost  :: CardCost
  } deriving (Eq, Typeable, Show, Ord)

data CardDescr = CardDescr
  { primary :: [ Effect ]
  , other   ::   String
  } deriving (Eq, Typeable, Show, Ord)

data Effect = Effect
  { amount     :: Int
  , effectType :: EffectType
  } deriving (Eq, Typeable, Show, Ord)

type CardCost = Int

type TurnID = String

data Turn = Turn
  { turnID     :: TurnID
  , turnPhases :: [Phase]
  } deriving (Eq, Typeable, Show, Ord)

data Phase = Phase
  { phaseName :: PhaseName
  , phaseInt  :: PhaseInt
  } deriving (Eq, Typeable, Show, Ord)

data PhaseInt = PhaseInt Int | All
  deriving (Eq, Typeable, Show, Ord)

data PhaseName = ActionP  | BuyP  | DiscardP | DrawP
  deriving (Eq, Typeable, Show, Ord)

-- Do we need this? -- RLV
-- qName :: QString -> String
-- qName [n] = n
-- qName (n:ms) = n ++ "." ++ qName ms
