module Game.UUAntGen.AntMoves where

import Data.List (intersperse)

import Game.UUAntGen.AntAssembly
import Game.UUAntGen.AntDeepEmbedded
import Game.UUAntGen.AntImperative
import Game.UUAntGen.AntInstruction


-- | Tries to move one step forward, uses the strategy passed as parameter if meets a wall
moveOrWall :: AntImperative -> AntImperative
moveOrWall wi = iIfThen (Not TryForward) wi


-- | Tries to move one step forward, does ABSOLUTELY NOTHING if meets a wall
move :: AntImperative
move = iTest TryForward


-- | Performs a given strategy in loop until a certain condition is met
doUntil :: AntImperative -> AntTest -> AntImperative
doUntil f t = iWhile (Not t) f


-- | Tries to move forward in a straight line until a condition is met. Has a "recovery"
--   strategy passed for when a wall is met midway through. Does the recovery and goes 
--   forward again
goFFUntilOrWall :: AntTest -> AntImperative -> AntImperative
goFFUntilOrWall t w = doUntil (moveOrWall w) t


-- | Goes forward until a condition is met. Doesn't care for walls in the way
goFFUntil :: AntTest -> AntImperative
goFFUntil t = doUntil move t


-- | Goes forward n number of steps. Doesn't care for walls in the way
goForwardNSteps :: Int -> AntImperative
goForwardNSteps n = iList (replicate n move)


-- | Makes an ant leave pheromone behind while performing any task
withPheromone :: Pheromone -> AntImperative -> AntImperative
withPheromone _ (Single r)         = Single r
withPheromone p (IfThenElse c t f) = IfThenElse c (withPheromone p t) (withPheromone p f)
withPheromone p (IfThen c b)       = IfThen c (withPheromone p b)
withPheromone p (While c b)        = While c (withPheromone p b)
withPheromone _ (SideEffect i)     = SideEffect i
withPheromone p (IList l)          = iList $ intersperse (iMark p) l



-- HIGHER LEVEL
type AntWalk = AntImperative -> AntImperative

-- | Moves forward n steps, performing some other strategy meanwhile
doForwardNStepsWith :: Int -> AntImperative -> AntImperative
doForwardNStepsWith n other = iList $ replicate n (move `iSeq` other)


-- | Turns 180 degrees
turnAround :: AntImperative
turnAround = iList $ replicate 3 iTurnR


-- | Performs a random turn (loop to the right with 20% chance of stopping)
randomTurn :: AntImperative
randomTurn = doUntil iTurnR (TryRandomEqZero 4)


-- | Opening spiral, not covering all squares. A closed spiral is complicated. 
--   Ends with a turn
goSpiralL = goSpiralD L
goSpiralR = goSpiralD R
goSpiral = goSpiralR


-- Do we need an empty strategy to solve this boilerplate?
goSpiralD :: Dexterity -> Int -> AntImperative
goSpiralD d = goSpiral' 1
    where                             -- Might be improved by using goForwardUntil
        goSpiral' i n | i < n       = goMoveNTurn d i `iSeq` 
                                      goMoveNTurn d i `iSeq` 
                                      goSpiral' (i+1) n
                      | otherwise   = goMoveNTurn d n `iSeq` goMoveNTurn d n

doSpiralD :: Dexterity -> Int -> AntImperative -> AntImperative
doSpiralD d n s = doSpiral' 1 n
    where                             -- Might be improved by using goForwardUntil
        doSpiral' i n | i < n       = doMoveNTurn d i s `iSeq` 
                                      doMoveNTurn d i s `iSeq` 
                                      doSpiral' (i+1) n
                      | otherwise   = doMoveNTurn d n s `iSeq` doMoveNTurn d n s


goMoveNTurn :: Dexterity -> Int -> AntImperative
goMoveNTurn d n = goForwardNSteps n `iSeq` iTurn d

doMoveNTurn :: Dexterity -> Int -> AntImperative -> AntImperative
doMoveNTurn d n s = doForwardNStepsWith n s `iSeq` iTurn d



-- Looks for the condition, and stops when it is found
goSearch :: AntImperative -> Condition -> AntImperative
goSearch s c = doUntil s (TrySense Ahead c)

doSearch :: AntImperative -> Condition -> AntImperative -> AntImperative
doSearch s c a = doUntil (s `iSeq` a) (TrySense Ahead c)


goSearchSpiral = goSearch (goSpiral 2)

goFFandBack :: AntImperative
goFFandBack = goSearch move Rock `iSeq` turnAround `iSeq` goSearch move Home

doFFandBack :: AntImperative -> AntImperative
doFFandBack s = doSearch move Rock s `iSeq` turnAround `iSeq` doSearch move Home s

