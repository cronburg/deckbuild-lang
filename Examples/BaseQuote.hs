{-# LANGUAGE TypeSynonymInstances, TemplateHaskell, QuasiQuotes,
             MultiParamTypeClasses, FlexibleInstances, UndecidableInstances,
             DeriveDataTypeable, ScopedTypeVariables #-}

module Examples.BaseQuote where

-- Importing the following was the most convenient.
-- Until we figure out the better way to do it, if at all.
import Language.DeckBuild.Syntax
import Language.DeckBuild.Quote

-- Example of all common cards:
-- http://dominionstrategy.com/card-lists/dominion-card-list/
[deck|
        card Cellar  :: Action {
          +1 actions
          "Discard any number of cards. +1 Card per card discarded"
        } costs 2

        card Chapel  :: Action {
          "Trash up to 4 cards from your hand"
        } costs 2

        card Moat :: Action {
          +2 cards
          "When another player plays an Attack card, "
          "you may reveal this from your hand."
          "If you do, you are unaffected by that Attack."
        } costs 2

        card Chancellor :: Action {
          +2 cards
          "When another player plays an Attack card, "
          "you may reveal this from your hand."
          "If you do, you are unaffected by that Attack."
        } costs 2

        card Village :: Action {
          +1 cards
          +2 actions
        } costs 3

        card Woodcutter :: Action {
          +1 buys
          +2 coins
        } costs 3

        card Workshop :: Action {
          "Gain a card costing up to 4 coins."
        } costs 3

        card Bureaucrat :: Action {
          "Gain a silver card; put it on top of your deck. "
          "Each other player reveals a Victory card from his hand "
          "and puts it on his deck (or reveals a hand with no Victory cards)."
        } costs 4

        card Feast :: Action {
          "Trash this card. Gain a card costing up to 5 coins."
        } costs 4

        card Gardens :: Victory {
          "Worth 1 Victory for every 10 cards in your deck (rounded down)."
        } costs 4

        card Militia :: Action {
          +2 coins
          "Each other player discards down to 3 cards in his hand."
        }

        |]
