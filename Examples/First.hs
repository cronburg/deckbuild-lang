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


tests = TestList[ TestLabel "Card01"  card01_test ]

card01_result = let result = parse cardFile "" "card Village :: Action { +1 actions -3 buys \"awesome village\" } costs 3"
                    in case result of
                        Right x -> x
                        Left _ -> []            
card01_expects = [ Card "Village" ACTION (CardDescr { primary = [ Effect { amount = 1, effectType = ACTIONS},
                                                                   Effect {amount = -3, effectType = BUYS } ],
                                                       other = "awesome village"}) 3 ]
card01_test = TestCase $ assertEqual
     "card01" card01_expects card01_result

