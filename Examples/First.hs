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

-- Regression expects to be run from the Examples directory.
test = runTestTT tests


tests = TestList[ TestLabel "Card01"  card01_test
                , TestLabel "Card02"  card02_test
                , TestLabel "Card03"  card03_test
                , TestLabel "Card04"  card04_test
                , TestLabel "Card05"  card05_test ]

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

