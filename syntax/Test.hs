-- Test.hs
module DeckBuildTest where
import Test.HUnit
import DeckBuildParser
import Text.Parsec
-- assertEqual String -> e1 -> e2 -> Pass/Fail | e1 is AS, e2 is parsing the grammar in this case 
turntest1 = TestCase (assertEqual 
						"For: dominion's standard turn rules." 
						"Right (Turn \"Dominion_Standard\" [Phase ActionP (PhaseInt 1),Phase BuyP (PhaseInt 1),Phase DiscardP All,Phase DrawP (PhaseInt 5)])" 
						(show (parse turnDecl "" "turn Dominion_Standard { action 1 buy 1 discard all draw 5 }")))

tests = TestList [TestLabel "turntest1" turntest1]

--Matt: I had to run with:
-- >>> :load "deckbuild-syntax.hs" "deckbuild-parser.hs" "main.hs" "Test.hs"
-- >>> :m DeckBuildTest 
-- >>> Test.HUnit.runTestTT DeckBuildTest.tests
