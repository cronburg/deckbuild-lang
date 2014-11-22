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
    es <- mapM genDeckExp ds :: Q [Exp]
    let body = NormalB $ ListE es
    let constructors = genCons ds
    return
      [ DataD [] (mkName "CardName") [] constructors []
      , FunD (mkName "kingdomCards") [Clause [] body []]
      ]

genCons ds = [NormalC (mkCardName d) [] | d <- ds, isCard d]

isCard (DeckDeclCard c) = True
isCard _                = False

mkCardName (DeckDeclCard (Card {cID = name})) = mkName $ map toUpper name
mkCardName _ = undefined

-- genDeckExp :: DeckDecl -> Q [DeckDecl]
genDeckExp e = liftD e --runQ [| e |]

