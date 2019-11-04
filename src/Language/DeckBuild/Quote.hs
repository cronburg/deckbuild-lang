{-# LANGUAGE DeriveDataTypeable,TemplateHaskell,ScopedTypeVariables,KindSignatures #-}
module Language.DeckBuild.Quote
    (deck)
    where

import Prelude hiding (exp, init)
import System.IO.Unsafe (unsafePerformIO)

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote (QuasiQuoter(..))

import qualified Language.DeckBuild.Parser as P
import qualified Text.Parsec.Prim   as PP

import Language.DeckBuild.CodeGen (make_deck_declaration)

-- Top-level QuasiQuoter
deck :: QuasiQuoter
deck = QuasiQuoter (error "parse expression")
                   (error "parse pattern")
                   (error "parse type")
                   dparse

-- Deck parser in quasiquotation monad
dparse :: String -> TH.Q [TH.Dec]
dparse input = do

  -- TODO: use debugging information (filename, line #, column) in parser
  loc <- TH.location
  let fileName = TH.loc_filename loc
  let (line,column) = TH.loc_start loc

  case P.parseDeckDecls fileName line column input of
    Left err -> unsafePerformIO $ fail $ show err
    Right x  -> make_deck_declaration x

