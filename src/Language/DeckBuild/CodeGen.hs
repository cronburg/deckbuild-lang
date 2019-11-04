{-# LANGUAGE TemplateHaskell,ScopedTypeVariables,KindSignatures #-}
module Language.DeckBuild.CodeGen
  (make_deck_declaration)
  where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.DeckBuild.Syntax
import Data.Char (toUpper)

deriv n = derivClause Nothing [conT $ mkName n]
things_derived = map deriv ["Eq", "Typeable", "Show", "Ord"]

make_deck_declaration :: [DeckDecl] -> Q [Dec]
make_deck_declaration ds = do
  let kingdomCardBody = normalB $ listE $ map genDeckExp $ filter isCard ds
  let turnRulesBody   = normalB $ listE $ map genDeckExp $ filter (not.isCard) ds
  cardName <- dataD (cxt []) (mkName "CardName") [] Nothing (genCons ds) things_derived
  runtimeCard <- dataD (cxt []) (mkName "RuntimeCard") [] Nothing genCardCons things_derived
  kingdomCards <- funD (mkName "kingdomCards") [clause [] kingdomCardBody []]
  turnRules <- funD (mkName "turnRules")    [clause [] turnRulesBody   []]
  return [cardName, runtimeCard, kingdomCards, turnRules]

genCons :: [DeckDecl] -> [Q Con]
genCons ds = [ normalC (mkCardName d) [] | d <- ds, isCard d ]

noBang :: Q Strict
noBang = bang noSourceUnpackedness sourceStrict

noBangType :: Q Type -> Q BangType
noBangType = bangType noBang

genCardCons :: [Q Con]
genCardCons = [ recC (mkName "RuntimeCard")
    [ varBangType (mkName "cID")    $ noBangType $ conT $ mkName "CardName"
    , varBangType (mkName "cType")  $ noBangType $ conT $ mkName "CardType"
    , varBangType (mkName "cDescr") $ noBangType $ conT $ mkName "CardDescr"
    , varBangType (mkName "cCost")  $ noBangType $ conT $ mkName "CardCost"
    ]
  ]

isCard :: DeckDecl -> Bool
isCard (DeckDeclCard c) = True
isCard _                = False

mkCardName :: DeckDecl -> Name
mkCardName (DeckDeclCard (Card {cID = name})) = mkName $ map toUpper name
mkCardName _ = undefined

genDeckExp :: DeckDecl -> Q Exp
genDeckExp e = liftD e --runQ [| e |]

