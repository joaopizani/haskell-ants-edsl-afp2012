module Game.UUAntGen.Test.AntInstructionHUnit where

import Test.HUnit ((~?=), test, (~:))
import qualified Data.Map as M
import Game.UUAntGen.AntMap (aDrop, getAntStrategy, AntStrategy'(..))
import Game.UUAntGen.AntInstruction


testDrop = "testDrop" ~:  getAntStrategy aDrop ~?= expected
    where
        z        = AntState 0
        expected = AntStrategy' (M.fromList [(z, Drop z)]) z z

