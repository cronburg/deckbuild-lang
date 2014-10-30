{-# LANGUAGE TypeSynonymInstances, TemplateHaskell, QuasiQuotes, 
             MultiParamTypeClasses, FlexibleInstances, UndecidableInstances,
             DeriveDataTypeable, ScopedTypeVariables #-}

module Examples.First where
-- TODO
-- import Language.DeckBuild.DeckBuildc
-- import Language.DeckBuild.Testing

import System.IO.Unsafe (unsafePerformIO)
import Data.Char as Char
import qualified Data.ByteString as B
import Data.Word

ws = REd "[\t ]+|$" " "

-- Regression expects to be run from the Examples directory.
test = runTestTT tests


tests = TestList[ TestLabel "Literal"  whiteSpace_test
                , TestLabel "Literal"  whiteSpace2_test
                ]

[deck|  |]

--
-- Code copied from the PADS test file. TODO
--
-- [deck| type IntPair = (Int, '|', Int) |]
-- intPair_result = intPair_parseS "12|23"
-- intPair_expects =  ((12,23), 0,"")
-- intPair_test = mkTestCase "intPair" intPair_expects intPair_result
-- 
-- [deck| type WhiteSpace = (Int, '[ \t]+', Int) |]
-- whiteSpace_input = "12      34"
-- whiteSpace_result = whiteSpace_parseS whiteSpace_input
-- whiteSpace_expects = ((12,34),0,"")
-- whiteSpace_test = mkTestCase "regular expression literal" whiteSpace_expects whiteSpace_result

