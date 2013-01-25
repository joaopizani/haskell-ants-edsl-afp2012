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

goForwardNSteps :: Int -> AntStrategy
goForwardNSteps n = replicateStrat n aMove

doForwardNSteps :: Int -> AntWalk
doForwardNSteps n f = replicateStrat n (aMove >>- f)

turnAround :: AntStrategy
turnAround = replicateStrat 3 aTurnR

randomTurn :: AntStrategy
randomTurn = goUntil aTurnR (TryRandomEqZero 5)

-- opening spiral, not covering all squares. the closed spiral is much more complicated.
-- ends with a turn
goSpiralL = goSpiralD L 
goSpiralR = goSpiralD R
goSpiral = goSpiralR

-- Do we need an empty strategy to solve this boilerplate?

doSpiralD :: Dexterity -> Int -> AntWalk
doSpiralD d n s = doSpiral' 1 n
    where                             -- Might be improved by using goForwardUntil 
        doSpiral' i n | i < n       = doMoveNTurn d i s >>- doMoveNTurn d i s >>- doSpiral' (i+1) n
                      | otherwise   = doMoveNTurn d n s >>- doMoveNTurn d n s

goSpiralD :: Dexterity -> Int -> AntStrategy
goSpiralD d = goSpiral' 1
    where                             -- Might be improved by using goForwardUntil 
        goSpiral' i n | i < n       = goMoveNTurn d i >>- goMoveNTurn d i >>- goSpiral' (i+1) n
                      | otherwise   = goMoveNTurn d n >>- goMoveNTurn d n

goMoveNTurn :: Dexterity -> Int -> AntStrategy 
goMoveNTurn d n = goForwardNSteps n >>- case d of 
                                        L -> aTurnL
                                        R -> aTurnR

doMoveNTurn :: Dexterity -> Int -> AntWalk
doMoveNTurn d n s = doForwardNSteps n s >>- case d of 
                                        L -> aTurnL
                                        R -> aTurnR


goUntil :: AntStrategy -> AntTest -> AntStrategy
goUntil f t = aWhile (Not $ t) f

-- Looks for the condition, and stops when it is found
goSearch :: AntStrategy -> Condition -> AntStrategy
goSearch f c = goUntil f (TrySense Ahead c)

doSearch :: AntStrategy -> Condition -> AntWalk
doSearch f c a = goUntil (f >>- a) (TrySense Ahead c)

goSearchSpiral = goSearch (goSpiral 2)

goFFUntilOrRock :: AntTest -> AntStrategy -> AntStrategy
goFFUntilOrRock t sw = goUntil (aMoveOrWall sw) t 

goFFUntil :: AntTest-> AntStrategy
goFFUntil t = goUntil aMove t

goFFandBack :: AntStrategy
goFFandBack = goSearch aMove Rock >>- turnAround >>- goSearch aMove Home

doFFandBack :: AntWalk
doFFandBack a = doSearch aMove Rock a >>- turnAround >>- doSearch aMove Home a

