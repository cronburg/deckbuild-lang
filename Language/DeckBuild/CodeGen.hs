{-# LANGUAGE TemplateHaskell #-}
module Language.DeckBuild.CodeGen
  (make_deck_declaration)
  where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.DeckBuild.Syntax

make_deck_declaration :: [DeckDecl] -> Q [Dec]
make_deck_declaration ds = do
    es <- mapM genDeckExp ds
    let body = NormalB $ ListE es
    return [ FunD (mkName "kingdomCards") [Clause [] body []] ]
--
--
--   DataD option:
--   let constructors = genCons ds
--   return $
--     [ DataD [] (mkName "Card") [] constructors []
--     ]

genCons ds = [NormalC (mkCardName d) [] | d <- ds, isCard d]

isCard (DeckDeclCard c) = True
isCard _                = False

mkCardName (DeckDeclCard (Card {cID = name})) = mkName name
mkCardName _ = undefined

-- genDeckExp :: DeckDecl -> Q [DeckDecl]
genDeckExp e = returnQ e --runQ [| e |]

