{-# LANGUAGE TemplateHaskell,ScopedTypeVariables #-}
module Language.DeckBuild.CodeGen
  (make_deck_declaration)
  where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.DeckBuild.Syntax
import Data.Char (toUpper)

make_deck_declaration :: [DeckDecl] -> Q [Dec]
make_deck_declaration ds = do
    card_es <- mapM genDeckExp $ filter      isCard  ds :: Q [Exp]
    turn_es <- mapM genDeckExp $ filter (not.isCard) ds
    let kingdomCardBody = NormalB $ ListE card_es
    let turnRulesBody   = NormalB $ ListE turn_es
    let constructors = genCons ds
    return
      [ DataD [] (mkName "CardName") [] constructors []
      , FunD (mkName "kingdomCards") [Clause [] kingdomCardBody []]
      , FunD (mkName "turnRules")    [Clause [] turnRulesBody   []]
      ]

genCons ds = [NormalC (mkCardName d) [] | d <- ds, isCard d]

isCard (DeckDeclCard c) = True
isCard _                = False

mkCardName (DeckDeclCard (Card {cID = name})) = mkName $ map toUpper name
mkCardName _ = undefined

-- genDeckExp :: DeckDecl -> Q [DeckDecl]
genDeckExp e = liftD e --runQ [| e |]

