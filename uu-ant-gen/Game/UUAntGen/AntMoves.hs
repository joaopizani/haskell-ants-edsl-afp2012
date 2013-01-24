module Game.UUAntGen.AntMoves where

import Game.UUAntGen.AntBasic
import Game.UUAntGen.AntImperative
import Game.UUAntGen.AntInstruction

import qualified Data.Map as M
import Control.Monad.Supply (supply)

aMoveOrWall :: AntStrategy -> AntStrategy
aMoveOrWall ws = aIfThen (Not TryForward) ws 

aMove :: AntStrategy 
aMove = aTest TryForward

goFFUntilOrRock :: AntTest -> AntStrategy -> AntStrategy
goFFUntilOrRock t sw = aWhile (Not t) (aMoveOrWall sw) 

goFFUntil :: AntTest-> AntStrategy
goFFUntil t = aWhile (Not t) aMove

goForwardNSteps :: Int -> AntStrategy
goForwardNSteps n = foldr (>>-) aMove $ replicate (n-1) aMove


-- opening spiral, not covering all squares. the closed spiral is much more complicated.
-- ends with a turn
goSpiralL = goSpiralD L
goSpiralR = goSpiralD R
goSpiral = goSpiralR

goSpiralD :: Dexterity -> Int -> AntStrategy
goSpiralD d = goSpiral' 1
    where                             -- Might be improved by using goForwardUntil 
        goSpiral' i n | i < n       = goMoveNTurn d i >>- goMoveNTurn d i >>- goSpiral' (i+1) n
                      | otherwise   = goMoveNTurn d n >>- goMoveNTurn d n

goMoveNTurn :: Dexterity -> Int -> AntStrategy
goMoveNTurn d n = goForwardNSteps n >>- case d of 
                                        L -> aTurnL
                                        R -> aTurnR

-- Looks for the condition, and stops when it is found
goSearch :: AntStrategy -> Condition -> AntStrategy
goSearch f c = aWhile (Not $ TrySense Ahead c) f 

goSearchSpiral = goSearch (goSpiral 2)
