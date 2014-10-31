{-# LANGUAGE TypeSynonymInstances, TemplateHaskell, QuasiQuotes, 
             MultiParamTypeClasses, FlexibleInstances, UndecidableInstances,
             DeriveDataTypeable, ScopedTypeVariables #-}

module Examples.First where

import System.IO.Unsafe (unsafePerformIO)
import Test.HUnit hiding (test)
-- import Text.ParserCombinators.Parsec

-- Importing the following was the most convenient.
-- Until we figure out the better way to do it, if at all.
import Language.DeckBuild.Syntax
import Language.DeckBuild.Parser

-- Regression expects to be run from the Examples directory.
test = runTestTT tests


tests = TestList[ TestLabel "Card01"  card01_test
                , TestLabel "Card02"  card02_test
                , TestLabel "Card03"  card03_test
                , TestLabel "Card04"  card04_test
                , TestLabel "Card05"  card05_test 
                , TestLabel "Turn01"  turn01_test
                , TestLabel "Turn02"  turn02_test
                , TestLabel "Turn03"  turn03_test
                , TestLabel "Turn04"  turn04_test
                , TestLabel "Turn05"  turn05_test ]

-- Basic card syntax.
card01_result = let result = parse cardFile "" "card Village :: Action { +1 actions -3 buys \"awesome village\" } costs 3"
                    in case result of
                        Right x -> x
                        Left _ -> []            
card01_expects = [ Card "Village" ACTION (CardDescr { primary = [ Effect { amount = 1, effectType = ACTIONS },
                                                                   Effect { amount = -3, effectType = BUYS } ],
                                                       other = "awesome village"}) 3 ]
card01_test = TestCase $ assertEqual
     "card01" card01_expects card01_result

-- Space in action numeric modifier
card02_result = let result = parse cardFile "" "card Village2 :: Action { + 1 actions -   3 buys \"awesome village\" } costs 3"
                    in case result of
                        Right x -> x
                        Left _ -> []            
card02_expects = [ Card "Village2" ACTION (CardDescr { primary = [ Effect { amount = 1, effectType = ACTIONS },
                                                                   Effect { amount = -3, effectType = BUYS } ],
                                                       other = "awesome village"}) 3 ]
card02_test = TestCase $ assertEqual
     "card02" card02_expects card02_result

-- Buy before action
-- Note that order in the Effect list is significant.
card03_result = let result = parse cardFile "" "card Village3 :: Action { +1 buys +3 actions \"awesome village\" } costs 3"
                    in case result of
                        Right x -> x
                        Left _ -> []            
card03_expects = [ Card "Village3" ACTION (CardDescr { primary = [ Effect { amount = 1, effectType = BUYS },
                                                                   Effect { amount = 3, effectType = ACTIONS } ],
                                                       other = "awesome village"}) 3 ]
card03_test = TestCase $ assertEqual
     "card03" card03_expects card03_result

-- Buy only. No action
card04_result = let result = parse cardFile "" "card Village4 :: Action { +100 buys  \"awesome village\" } costs 3"
                    in case result of
                        Right x -> x
                        Left _ -> []            
card04_expects = [ Card "Village4" ACTION (CardDescr { primary = [ Effect { amount = 100, effectType = BUYS } ],
                                                       other = "awesome village"}) 3 ]
card04_test = TestCase $ assertEqual
     "card04" card04_expects card04_result

-- Action only. No buys.
card05_result = let result = parse cardFile "" "card Village5 :: Action { +200 actions  \"awesome village\" } costs 3"
                    in case result of
                        Right x -> x
                        Left _ -> []            
card05_expects = [ Card "Village5" ACTION (CardDescr { primary = [ Effect { amount = 200, effectType = ACTIONS } ],
                                                       other = "awesome village"}) 3 ]
card05_test = TestCase $ assertEqual
     "card05" card05_expects card05_result
--standard Dominion turn ruleset     
turn01_result = let result = parse turnDecl "" "turn Dominion_Standard { action 1 buy 1 discard all draw 5 }"
                in case result of
                   Right x -> x
                   Left _ -> Turn "" []

turn01_expects = (Turn "Dominion_Standard" [Phase ActionP (PhaseInt 1),Phase BuyP (PhaseInt 1),Phase DiscardP All,Phase DrawP (PhaseInt 5)])
turn01_test = TestCase $ assertEqual 
    "turn01" turn01_expects turn01_result
turn02_result = let result = parse turnDecl "" "turn Easy_Mode { action 2 buy 2 discard all draw 7 }"
                in case result of
                   Right x -> x
                   Left _ -> Turn "" []

turn02_expects = (Turn "Easy_Mode" [Phase ActionP (PhaseInt 2),Phase BuyP (PhaseInt 2),Phase DiscardP All,Phase DrawP (PhaseInt 7)])
turn02_test = TestCase $ assertEqual 
    "turn02" turn02_expects turn02_result
turn03_result = let result = parse turnDecl "" "turn HardCore { action 1 buy 1 discard all draw 1 }"
                in case result of
                   Right x -> x
                   Left _ -> Turn "" []

turn03_expects = (Turn "HardCore" [Phase ActionP (PhaseInt 1),Phase BuyP (PhaseInt 1),Phase DiscardP All,Phase DrawP (PhaseInt 1)])
turn03_test = TestCase $ assertEqual 
    "turn03" turn03_expects turn03_result
turn04_result = let result = parse turnDecl "" "turn Garbage_Collector { action 2 buy 1 discard 3 draw 4 }"
                in case result of
                   Right x -> x
                   Left _ -> Turn "" []

turn04_expects = (Turn "Garbage_Collector" [Phase ActionP (PhaseInt 2),Phase BuyP (PhaseInt 1),Phase DiscardP (PhaseInt 3),Phase DrawP (PhaseInt 4)])
turn04_test = TestCase $ assertEqual 
    "turn04" turn04_expects turn04_result
turn05_result = let result = parse ruleFile "" "turn Dominion_Standard { action 1 buy 1 discard all draw 5 } turn Easy_Mode { action 2 buy 2 discard all draw 7 } turn HardCore { action 1 buy 1 discard all draw 1 } turn Garbage_Collector { action 2 buy 1 discard 3 draw 4 }"
                in case result of
                   Right x -> x
                   Left _ -> []

turn05_expects = [Turn "Dominion_Standard" [Phase ActionP (PhaseInt 1),Phase BuyP (PhaseInt 1),Phase DiscardP All,Phase DrawP (PhaseInt 5)],Turn "Easy_Mode" [Phase ActionP (PhaseInt 2),Phase BuyP (PhaseInt 2),Phase DiscardP All,Phase DrawP (PhaseInt 7)],Turn "HardCore" [Phase ActionP (PhaseInt 1),Phase BuyP (PhaseInt 1),Phase DiscardP All,Phase DrawP (PhaseInt 1)],Turn "Garbage_Collector" [Phase ActionP (PhaseInt 2),Phase BuyP (PhaseInt 1),Phase DiscardP (PhaseInt 3),Phase DrawP (PhaseInt 4)]]
turn05_test = TestCase $ assertEqual 
    "turn05" turn05_expects turn05_result



