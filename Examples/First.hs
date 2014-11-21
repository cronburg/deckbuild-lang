{-# LANGUAGE TypeSynonymInstances, TemplateHaskell, QuasiQuotes,
             MultiParamTypeClasses, FlexibleInstances, UndecidableInstances,
             DeriveDataTypeable, ScopedTypeVariables #-}

module Examples.First where

import System.IO.Unsafe (unsafePerformIO)
import Test.HUnit hiding (test)
import Text.ParserCombinators.Parsec

-- Importing the following was the most convenient.
-- Until we figure out the better way to do it, if at all.
import Language.DeckBuild.Syntax
import Language.DeckBuild.Parser

-------------------------------------------------------------------------------
-- Regression expects to be run from the Examples directory.
test = runTestTT tests
tests = TestList
  [ TestLabel "Card01"  card01_test
  , TestLabel "Card02"  card02_test
  , TestLabel "Card03"  card03_test
  , TestLabel "Card04"  card04_test
  , TestLabel "Card05"  card05_test
  , TestLabel "Turn01"  turn01_test
  , TestLabel "Turn02"  turn02_test
  , TestLabel "Turn03"  turn03_test
  , TestLabel "Turn04"  turn04_test
  , TestLabel "Turn05"  turn05_test
  , TestLabel "Deck01"  deck01_test
  ]

-------------------------------------------------------------------------------
-- Macros

getResult' _    (Right  x) = x
getResult' null (Left err) = unsafePerformIO $ (putStr $ show err) >> return null

getResult = getResult' []
getTurn   = getResult' $ Turn "ERROR" []

--getTurn   (Right  x) = x
--getResult (Left err) = unsafePerformIO $ (putStr $ show err) >> return

mkTest   s e r = TestCase $ assertEqual s e r
mkTestIO s e r = TestCase $ assertEqual s e (unsafePerformIO r)

-------------------------------------------------------------------------------
-- Tests

deck01_input   = "card Village  :: Action { +1 actions -3     buys \"awesome village\" } costs 3 "
              ++ "card Village2 :: Action { + 1 actions -   3 buys \"awesome village\" } costs 3 "
deck01_result  = getResult $ parse deckDecls "" deck01_input
deck01_test    = mkTest "deck01" deck01_expects deck01_result
deck01_expects = map DeckDeclCard $ card01_expects ++ card02_expects

-- Basic card syntax.
card01_input   = "card Village :: Action { +1 actions -3 buys \"awesome village\" } costs 3"
card01_result  = getResult $ parse cardFile "" card01_input
card01_test    = mkTest "card01" card01_expects card01_result
card01_expects =
  [ Card
    { cID    = "Village"
    , cType  = ACTION
    , cCost  = 3
    , cDescr = CardDescr
      { primary = [ Effect { amount = 1,  effectType = ACTIONS }
                  , Effect { amount = -3, effectType = BUYS    } ]
      , other   = "awesome village" }
    }
  ]

-- Space in action numeric modifier
card02_input   = "card Village2 :: Action { + 1 actions -   3 buys \"awesome village\" } costs 3"
card02_result  = getResult $ parse cardFile "" card02_input
card02_test    = mkTest "card02" card02_expects card02_result
card02_expects =
  [ Card
    { cID    = "Village2"
    , cType  = ACTION
    , cCost  = 3
    , cDescr = CardDescr
      { primary = [ Effect { amount = 1,  effectType = ACTIONS }
                  , Effect { amount = -3, effectType = BUYS    } ]
      , other   = "awesome village" }
    }
  ]

-- Buy before action
-- Note that order in the Effect list is significant.
card03_input   = "card Village3 :: Action { +1 buys +3 actions \"awesome village\" } costs 3"
card03_result  = getResult $ parse cardFile "" card03_input
card03_test    = mkTest "card03" card03_expects card03_result
card03_expects =
  [ Card
    { cID    = "Village3"
    , cType  = ACTION
    , cCost  = 3
    , cDescr = CardDescr
      { primary = [ Effect { amount = 1, effectType = BUYS    }
                  , Effect { amount = 3, effectType = ACTIONS } ]
      , other   = "awesome village" }
    }
  ]

-- The effect of this card gives additional buys (no additional actions as in test cases above)
card04_input   = "card Village4 :: Action { +100 buys  \"awesome village\" } costs 3"
card04_result  = getResult $ parse cardFile "" card04_input
card04_test    = mkTest "card04" card04_expects card04_result
card04_expects =
  [ Card
    { cID    = "Village4"
    , cType  = ACTION
    , cCost  = 3
    , cDescr = CardDescr
      { primary = [ Effect { amount = 100, effectType = BUYS } ]
      , other   = "awesome village" }
    }
  ]

-- Card effect gives additional actions (no additional buys)
card05_input   = "card Village5 :: Action { +200 actions  \"awesome village\" } costs 3"
card05_result  = getResult $ parse cardFile "" card05_input
card05_test    = mkTest "card05" card05_expects card05_result
card05_expects =
  [ Card
    { cID    = "Village5"
    , cType  = ACTION
    , cCost  = 3
    , cDescr = CardDescr
      { primary = [ Effect { amount = 200, effectType = ACTIONS } ]
      , other = "awesome village"}
    }
  ]

-- standard Dominion turn ruleset
turn01_input   = "turn Dominion_Standard { action 1 buy 1 discard all draw 5 }"
turn01_result  = getTurn $ parse turnDecl "" turn01_input
turn01_test    = mkTest "turn01" turn01_expects turn01_result
turn01_expects =
  Turn "Dominion_Standard"
    [ Phase ActionP  (PhaseInt 1)
    , Phase BuyP     (PhaseInt 1)
    , Phase DiscardP  All
    , Phase DrawP    (PhaseInt 5) ]

turn02_input   = "turn Easy_Mode { action 2 buy 2 discard all draw 7 }"
turn02_result  = getTurn $ parse turnDecl "" turn02_input
turn02_test    = mkTest "turn02" turn02_expects turn02_result
turn02_expects =
  Turn "Easy_Mode"
    [ Phase ActionP (PhaseInt 2)
    , Phase BuyP (PhaseInt 2)
    , Phase DiscardP All
    , Phase DrawP (PhaseInt 7) ]

turn03_input   = "turn HardCore { action 1 buy 1 discard all draw 1 }"
turn03_result  = getTurn $ parse turnDecl "" turn03_input
turn03_test    = mkTest "turn03" turn03_expects turn03_result
turn03_expects = Turn "HardCore"
    [ Phase ActionP (PhaseInt 1)
    , Phase BuyP (PhaseInt 1)
    , Phase DiscardP All
    , Phase DrawP (PhaseInt 1) ]

turn04_input   = "turn Garbage_Collector { action 2 buy 1 discard 3 draw 4 }"
turn04_result  = getTurn $ parse turnDecl "" turn04_input
turn04_test    = mkTest "turn04" turn04_expects turn04_result
turn04_expects = Turn "Garbage_Collector"
    [ Phase ActionP (PhaseInt 2)
    , Phase BuyP (PhaseInt 1)
    , Phase DiscardP (PhaseInt 3)
    , Phase DrawP (PhaseInt 4) ]

-- Maybe this should go in its own file?
  -- could also do e.g. `turn04_input ++ turn03_input ++ ...`
turn05_input   = "turn Dominion_Standard { action 1 buy 1 discard all draw 5 } "
              ++ "turn Easy_Mode { action 2 buy 2 discard all draw 7 } turn HardCore "
              ++ "{ action 1 buy 1 discard all draw 1 } turn Garbage_Collector { "
              ++ "action 2 buy 1 discard 3 draw 4 }"
turn05_result  = getResult $ parse ruleFile "" turn05_input
turn05_test    = mkTest "turn05" turn05_expects turn05_result
turn05_expects =
  [Turn "Dominion_Standard"
    [ Phase ActionP  (PhaseInt 1)
    , Phase BuyP     (PhaseInt 1)
    , Phase DiscardP  All
    , Phase DrawP    (PhaseInt 5) ]
  ,Turn "Easy_Mode"
    [ Phase ActionP  (PhaseInt 2)
    , Phase BuyP     (PhaseInt 2)
    , Phase DiscardP  All
    , Phase DrawP    (PhaseInt 7) ]
  , Turn "HardCore"
    [ Phase ActionP  (PhaseInt 1)
    , Phase BuyP     (PhaseInt 1)
    , Phase DiscardP  All
    , Phase DrawP    (PhaseInt 1) ]
  , Turn "Garbage_Collector"
    [ Phase ActionP  (PhaseInt 2)
    , Phase BuyP     (PhaseInt 1)
    , Phase DiscardP (PhaseInt 3)
    , Phase DrawP    (PhaseInt 4) ]
  ]

