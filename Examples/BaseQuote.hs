{-# LANGUAGE TypeSynonymInstances, TemplateHaskell, QuasiQuotes,
             MultiParamTypeClasses, FlexibleInstances, UndecidableInstances,
             DeriveDataTypeable, ScopedTypeVariables #-}

module Examples.BaseQuote where

-- Importing the following was the most convenient.
-- Until we figure out the better way to do it, if at all.
import Language.DeckBuild.Syntax
import Language.DeckBuild.Quote

[deck|
        card Cellar  :: Action {
          +1 actions +1 buys 
          "Discard any number of cards. +1 Card per card discarded"
        } costs 2
        
        card Chapel  :: Action {
          "Trash up to 4 cards from your hand."
        }

        card Moat    :: Action {
          +2 cards
          "When another player plays an Attack card, you may "
          "reveal this from your hand. If you do, you are unaffected "
          "by that Attack. "
        }
     |]

