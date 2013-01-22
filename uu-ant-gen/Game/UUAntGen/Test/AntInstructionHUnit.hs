module Game.UUAntGen.Test.AntInstructionHUnit where

import qualified Data.HashMap.Lazy            as M
import           Game.UUAntGen.AntInstruction
import           Game.UUAntGen.AntMap         (AntStrategy' (..), aDrop, aTurnL, getAntStrategy)
import           Test.HUnit                   (Test, test, (~:), (~?=))


testAntInstruction :: Test
testAntInstruction = "testAntInstruction" ~: test $
    [ testDrop
    , testTurnL ]


s0, s1, s2, s3, s4, s5 :: AntState
s0 = AntState 0
s1 = AntState 1
s2 = AntState 2
s3 = AntState 3
s4 = AntState 4
s5 = AntState 5


testDrop :: Test
testDrop = "testDrop" ~:  getAntStrategy aDrop ~?= expected
    where expected = AntStrategy' (M.fromList [(s0, Drop s0)]) s0 s0


testTurnL :: Test
testTurnL = "testTurnL" ~:  getAntStrategy aTurnL ~?= expected
    where expected = AntStrategy' (M.fromList [(s0, Turn L s0)]) s0 s0

