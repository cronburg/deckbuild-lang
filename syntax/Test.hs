-- Test.hs
module DeckBuildTest where
import Test.HUnit
import DeckBuildParser
import Text.Parsec
import Language.DeckBuild.Syntax
import Text.ParserCombinators.Parsec

-- assertEqual String -> e1 -> e2 -> Pass/Fail | e1 is AS, e2 is parsing the grammar in this case 
turnresult1 = let result = parse turnDecl "" "turn Dominion_Standard { action 1 buy 1 discard all draw 5 }"
                in case result of
                   Right x -> x
                   Left _ -> Turn "" []

turnexpect1 = (Turn "Dominion_Standard" [Phase ActionP (PhaseInt 1),Phase BuyP (PhaseInt 1),Phase DiscardP All,Phase DrawP (PhaseInt 5)])
turntest1 = TestCase $ assertEqual 
						"For: dominion's standard turn rules." 
						turnresult1
						turnexpect1

tests = TestList [TestLabel "turntest1" turntest1]

--Matt: I had to run with:
-- >>> :load "deckbuild-syntax.hs" "deckbuild-parser.hs" "Test.hs"
-- >>> :m DeckBuildTest 
-- >>> Test.HUnit.runTestTT DeckBuildTest.tests
