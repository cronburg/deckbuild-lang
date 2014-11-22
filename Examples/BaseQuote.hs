{-# LANGUAGE TypeSynonymInstances, TemplateHaskell, QuasiQuotes,
             MultiParamTypeClasses, FlexibleInstances, UndecidableInstances,
             DeriveDataTypeable, ScopedTypeVariables #-}

module Examples.BaseQuote where

-- Importing the following was the most convenient.
-- Until we figure out the better way to do it, if at all.
  -- We'll definitely want to make some cleaner interfaces at some point
  -- to our runtime types and quasiquoter - at the moment you have to
  -- manually hide certain things from the Syntax: -KLC-
import Language.DeckBuild.Syntax hiding (cID, cType, cDescr, cCost)
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
        } costs 4

        card Moneynder :: Action {
          "Trash a Copper from your hand. If you do, +3 coins."
        } costs 4

        card Remodel :: Action {
          "Trash a card from your hand. Gain a card costing up to "
          "2 coins more than the trashed card."
        } costs 4

        card Smithy :: Action {
          +3 cards
        } costs 4

        card Spy :: Action {
          +1 cards
          +1 actions
          "Each player (including you) reveals the top card of his deck "
          "and either discards it or puts it back, your choice."
        } costs 4

        card Thief :: Action {
          "Each other player reveals the top 2 cards of his deck. "
          "If they revealed any Treasure cards, "
          "they trash one of them that you choose. "
          "You may gain any or all of these trashed cards. "
          "They discard the other revealed cards."
        } costs 4

        card Throne_Room :: Action {
          "Choose an Action card in your hand. Play it twice."
        } costs 4

        card CouncilRoom :: Action {
          +4 cards
          +1 buys
          "Each other player draws a card."
        } costs 5

        card Festival :: Action {
          +2 actions
          +1 buys
          +2 coins
        } costs 5

        card Laboratory :: Action {
          +2 cards
          +2 actions
        } costs 5

        card Library :: Action {
          "Draw until you have 7 cards in hand. "
          "You may set aside any Action cards drawn this way, "
          "as you draw them; discard the set aside cards "
          "after you finish drawing."
        } costs 5

        card Market :: Action {
          +1 cards
          +1 actions
          +1 buys
          +1 coins
        } costs 5

        card Mine :: Action {
          "Trash a Treasure card from your hand. "
          "Gain a Treasure card costing up to 3 coins more; "
          "put it into your hand."
        } costs 5

        card Witch :: Action {
          +2 cards
          "Each other player gains a Curse card."
        } costs 5

        card Adventurer :: Action {
          "Reveal cards from your deck until you reveal 2 Treasure cards. "
          "Put those Treasure cards in your hand "
          "and discard the other revealed cards."
        } costs 6

        card Copper :: Treasure {
          +1 coins
        } costs 0

        card Silver :: Treasure {
          +2 coins
        } costs 3

        card Gold :: Treasure {
          +3 coins
        } costs 6

        card Estate :: Victory {
          +1 victory
        } costs 2

        card Duchy :: Victory {
          +3 victory
        } costs 5

        card Province :: Victory {
          +6 victory
        } costs 8

        card Curse :: Victory {
          -1 victory
        } costs 0

        turn Dominion_Standard {
            action 1
            buy 1
            discard all
            draw 5
        }

        |]
