{-# LANGUAGE TemplateHaskell #-}
module Language.DeckBuild.CodeGen
  (make_deck_declaration)
  where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.DeckBuild.Syntax

make_deck_declaration :: [DeckDecl] -> Q [Dec]
make_deck_declaration ds = do
--  es <- mapM genDeckExp ds
--  let body = NormalB $ ListE es
  let constructors = genCons ds
  return $
    [ DataD [] (mkName "Card") [] constructors []
    ]
--    [ FunD (mkName "kingdomCards") [Clause [] body []] ]

genCons ds = [NormalC (mkCardName d) [] | d <- ds, isCard d]

isCard (DeckDeclCard c) = True
isCard _                = False

mkCardName (DeckDeclCard (Card {cID = name})) = mkName name
mkCardName _ = undefined

--genDeckExp :: DeckDecl -> Q Exp
--genDeckExp e = undefined --runQ [| e |]

