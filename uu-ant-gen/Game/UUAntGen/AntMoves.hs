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

-- TODO should also be save from foes using OR
safeMove :: AntImperative
safeMove = (iWhile (TrySense Ahead Friend) 
                   (chooseUniformly [ iTurnR `iSeq` iTurnL
                                    , iList $ [ iTurnR, move  , turnAround
                                              , move  , iTurnR, iTurnR     ]])) `iSeq` 
           move

pickup :: AntImperative
pickup = iTest TryPickUp

dropFood = Single CDrop

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
goFFUntil t = doUntil safeMove t


-- | Goes forward n number of steps. Doesn't care for walls in the way
goForwardNSteps :: Int -> AntImperative
goForwardNSteps n = iList (replicate n safeMove)


-- | Makes an ant leave pheromone behind while performing any task
interleaveStrategy :: AntImperative -> AntImperative -> AntImperative
interleaveStrategy _ (Single r)         = Single r
interleaveStrategy s (IfThenElse c t f) = IfThenElse c (interleaveStrategy s t) 
                                                       (interleaveStrategy s f)
interleaveStrategy s (IfThen c b)       = IfThen c (interleaveStrategy s b)
interleaveStrategy s (While c b)        = While c (interleaveStrategy s b)
interleaveStrategy _ (SideEffect i)     = SideEffect i
interleaveStrategy s (IList l)          = iList $ intersperse s l


withPheromone :: Pheromone -> AntImperative -> AntImperative
withPheromone = interleaveStrategy . iMark

-- Chooses a random strategy, among the ones in the given list, with uniform distribution
chooseUniformly :: [AntImperative] -> AntImperative
chooseUniformly (s:[])   = s
chooseUniformly l@(s:ss) = iIfThenElse (TryRandomEqZero (sz)) s (chooseUniformly ss)
    where sz = length l

doWithChance :: Int -> AntImperative -> AntImperative
doWithChance p = iIfThen (Not $ TryRandomEqZero p)

oneOfOrNothing :: [AntImperative] -> AntImperative
oneOfOrNothing ss = doWithChance ((length ss)+1) $ chooseUniformly ss


-- HIGHER LEVEL
type AntWalk = AntImperative -> AntImperative

-- | Moves forward n steps, performing some other strategy meanwhile
doForwardNStepsWith :: Int -> AntImperative -> AntImperative
doForwardNStepsWith n other = interleaveStrategy other (goForwardNSteps n)


-- | Turns 180 degrees
turnAround :: AntImperative
turnAround = iList $ replicate 3 iTurnR


-- | Performs a random turn (loop to the right with 20% chance of stopping)
randomTurn :: AntImperative
randomTurn = oneOfOrNothing [turn60L, turn120L, turn180L, turn60R, turn120R]

turn60L  = iTurnL
turn120L = iTurnL `iSeq` iTurnL
turn180L = iTurnL `iSeq` iTurnL `iSeq` iTurnL
turn60R  = iTurnR
turn120R = iTurnR `iSeq` iTurnR


-- | Opening spiral, not covering all squares. A closed spiral is complicated. 
-- Ends with a turn
goSpiralL :: Int -> AntImperative
goSpiralL = goSpiralD L

goSpiralR :: Int -> AntImperative
goSpiralR = goSpiralD R

goSpiral :: Int -> AntImperative
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
        doSpiral' i k | i < k       = doMoveNTurn d i s `iSeq` 
                                      doMoveNTurn d i s `iSeq` 
                                      doSpiral' (i+1) k
                      | otherwise   = doMoveNTurn d k s `iSeq` doMoveNTurn d k s


goMoveNTurn :: Dexterity -> Int -> AntImperative
goMoveNTurn d n = goForwardNSteps n `iSeq` iTurn d

doMoveNTurn :: Dexterity -> Int -> AntImperative -> AntImperative
doMoveNTurn d n s = doForwardNStepsWith n s `iSeq` iTurn d

-- Looks for the condition, and stops when it is found
goSearch :: AntImperative -> Condition -> Direction -> AntImperative
goSearch s c d = doUntil s (TrySense d c)

doSearch :: AntImperative -> Condition -> Direction-> AntImperative -> AntImperative
doSearch s c d a = doUntil (s `iSeq` a) (TrySense d c)

goSearchSpiral :: Condition -> AntImperative
goSearchSpiral c = goSearch (goSpiral 4) c Here

goSearchSpiral' :: AntTest -> AntImperative
goSearchSpiral' = doUntil (goSpiral 4) 

goFFandBack :: AntImperative
goFFandBack = iList [ goSearch safeMove Rock Ahead 
                    , turnAround 
                    , goSearch move Home Here ]

doFFandBack :: AntImperative -> AntImperative
doFFandBack s = iList [ doSearch safeMove Rock Ahead s
                      , turnAround
                      , doSearch move Home Here s ]

markHome :: AntImperative
markHome = (doSearch move Rock Ahead (iMark P1)) `iSeq` 
           iList [turnAround, move, iTurnR, move, iTurnL, 
                  iWhile (And (Not $ TrySense Here Home) (TrySense LeftAhead (Marker P1)))
                         (iMark P2 `iSeq` safeMove)]

findTrail :: AntImperative
findTrail = iWhile (Not $ (And (Not $ TrySense Here (Marker P2)) (Not $ TrySense Here Home))) (goSpiral 2) 

findWayHome :: AntImperative
findWayHome = iIfThenElse (TrySense Here (Marker P2)) 
                          ((iIfThen (TrySense RightAhead (Marker P1) ) turnAround) `iSeq` 
                            goSearch safeMove Home Here) 
                          findTrail

gatherFood = findFood `iSeq` bringFoodHome

bringFoodHome = iIfThenElse (TrySense Here Food) 
                            (pickup `iSeq` findWayHome `iSeq` dropFood) 
                            findFood

findFood = goSearchSpiral Food

findFoodSampleMap = iList $ [ iTurnR, iTurnR, goFFUntil (TrySense Here Food), pickup 
                            , turnAround, goFFUntil (TrySense Here Home), dropFood ]
