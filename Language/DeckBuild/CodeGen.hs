module Language.DeckBuild.CodeGen
  (make_deck_declaration)
  where

import qualified Language.Haskell.TH as TH
import Language.DeckBuild.Syntax

make_deck_declaration :: [DeckDecl] -> TH.Q [TH.Dec]
make_deck_declaration ds = undefined

