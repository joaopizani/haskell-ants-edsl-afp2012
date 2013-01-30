module Game.UUAntGen.Frontend.AntMoves where

import Data.List (intersperse)

import Game.UUAntGen.Backend.AntAssembly
import Game.UUAntGen.Backend.AntDeepEmbedded
import Game.UUAntGen.Backend.AntInstruction

import Game.UUAntGen.Frontend.AntImperative


-- REALLY BASIC MOVES

-- Turns
turn60L, turn120L, turn180L, turn60R, turn120R :: AntImperative
turn60L  = iTurnL
turn120L = iTurnL `iSeq` iTurnL
turn180L = iTurnL `iSeq` iTurnL `iSeq` iTurnL
turn60R  = iTurnR
turn120R = iTurnR `iSeq` iTurnR


-- | Turns 180 degrees
turnAround :: AntImperative
turnAround = turn180L


-- | Picks up food (never pickup food at the nest (home))
pickup :: AntImperative
pickup = iTest TryPickUp


-- | Drops food
dropFood :: AntImperative
dropFood = Single CDrop


-- | Tries to move one step forward, does ABSOLUTELY NOTHING if meets a wall
move :: AntImperative
move = iTest TryForward


-- | Safe move. Works around friends or foes in front of it. Subject to RACE CONDITIONS
safeMove :: AntImperative
safeMove = iWhile (friendOrFoe Ahead) (iCase [tryLA, tryRA, (tautology, randDelay)]) `iSeq` move
    where
        tryLA                = (Not $ friendOrFoe LeftAhead,  detour (turn60L, turn120L))
        tryRA                = (Not $ friendOrFoe RightAhead, detour (turn60R, turn120R))
        randDelay            = doWithChance 1 iEmpty
        detour (goIn, goOut) = iList [goIn, move, turnAround, waitForFreeCell, move, goOut]
        friendOrFoe d        = (senseFriend d `Or` senseFoe d)
        waitForFreeCell      = iWhile (friendOrFoe Ahead) iEmpty


-- | Tries to move one step forward, uses the strategy passed as parameter if meets a wall
moveOrWall :: AntImperative -> AntImperative
moveOrWall wi = iIfThen (Not TryForward) wi





-- SIMPLY ITERATED MOTIONS

-- | Performs a given strategy until a certain condition is met
doUntil :: AntImperative -> AntTest -> AntImperative
doUntil f t = iWhile (Not t) f


-- | Tries to go forward straight until a condition is met. Has a "recovery" strategy passed
-- for when a wall is met midway through. Does the recovery and goes forward again
goFFUntilOrWall :: AntTest -> AntImperative -> AntImperative
goFFUntilOrWall t w = doUntil (moveOrWall w) t


-- | Forward until a condition is met. Doesn't care for walls. UNSAFE (collision-unsafe)
goFFUntil :: AntTest -> AntImperative
goFFUntil t = doUntil move t

-- | Forward until a condition is met. Doesn't care for walls. SAFE (collision-safe)
safeGoFFUntil :: AntTest -> AntImperative
safeGoFFUntil t = doUntil safeMove t


-- | Forward n steps. Doesn't care for walls. UNSAFE (collision-unsafe)
goFFNSteps :: Int -> AntImperative
goFFNSteps n = iList (replicate n move)

-- | Forward n steps. Doesn't care for walls. SAFE (collision-safe)
safeGoFFNSteps :: Int -> AntImperative
safeGoFFNSteps n = iList (replicate n safeMove)


goMoveNTurn :: Dexterity -> Int -> AntImperative
goMoveNTurn d n = safeGoFFNSteps n `iSeq` iTurn d


doMoveNTurn :: Dexterity -> Int -> AntImperative -> AntImperative
doMoveNTurn d n s = doFFNStepsWith n s `iSeq` iTurn d


-- | Looks for the condition, and stops when it is found
goSearch :: AntImperative -> Condition -> Direction -> AntImperative
goSearch s c d = doUntil s (TrySense d c)

-- | Same as goSearch, but with a second strategy interleaved
doSearch :: AntImperative -> Condition -> Direction -> AntImperative -> AntImperative
doSearch s c d a = doUntil (s `iSeq` a) (TrySense d c)




-- INTERLEAVING STRATEGIES

-- | Interleaves strategies S1 and S2. Introduces S1 between every pair of instructions in S2
interleaveStrategy :: AntImperative -> AntImperative -> AntImperative
interleaveStrategy _ (Single r)         = Single r
interleaveStrategy s (IfThenElse c t f) = IfThenElse c (interleaveStrategy s t) (interleaveStrategy s f)
interleaveStrategy s (IfThen c b)       = IfThen c (interleaveStrategy s b)
interleaveStrategy s (While c b)        = While c (interleaveStrategy s b)
interleaveStrategy _ (SideEffect i)     = SideEffect i
interleaveStrategy s (IList l)          = iList $ intersperse s l


-- | Interleaves a strategy with marking the ground using a pheromone
withPheromone :: Pheromone -> AntImperative -> AntImperative
withPheromone = interleaveStrategy . iMark




-- RANDOM CHOICE

-- | Performs an action with probability p/(p+1). P=0 is 1/1, P=1 is 1/2, P=2 is 2/3, P=3 is 3/4
doWithChance :: Int -> AntImperative -> AntImperative
doWithChance p = iIfThen (Not $ TryRandomEqZero p)


-- | Chooses a random strategy, among the ones in the given list, with uniform distribution
chooseUniformly :: [AntImperative] -> AntImperative
chooseUniformly (s:[])   = s
chooseUniformly l@(s:ss) = iIfThenElse (TryRandomEqZero sz) s (chooseUniformly ss)
    where sz = length l


-- | Given a list of length n, gives a chance of 1/(n+1) for each strategy in the list to
-- be executed, as well as a chance of 1/(n+1) that nothing will be done (empty strategy)
oneOfOrNothing :: [AntImperative] -> AntImperative
oneOfOrNothing ss = doWithChance ((length ss) + 1) $ chooseUniformly ss




-- | Performs a random turn
randomTurn :: AntImperative
randomTurn = oneOfOrNothing [turn60L, turn120L, turn180L, turn60R, turn120R]


-- | Moves forward n steps, performing some other strategy meanwhile
doFFNStepsWith :: Int -> AntImperative -> AntImperative
doFFNStepsWith n other = interleaveStrategy other (safeGoFFNSteps n)




-- CONVENIENCE FUNCTIONS, smart constructors, etc.
sense :: Direction -> Condition -> AntTest
sense = TrySense


senseMarker :: Direction -> Pheromone -> AntTest
senseMarker d p = sense d (Marker p)

senseMarkerHere :: Pheromone -> AntTest
senseMarkerHere = senseMarker Here


senseHome :: Direction -> AntTest
senseHome d = sense d Home

senseHomeHere :: AntTest
senseHomeHere = senseHome Here


senseFood :: Direction -> AntTest
senseFood d = sense d Food

senseFoodHere :: AntTest
senseFoodHere = senseFood Here


senseRock :: Direction -> AntTest
senseRock d = sense d Rock

senseRockHere :: AntTest
senseRockHere = senseRock Here


senseFoe :: Direction -> AntTest
senseFoe d = sense d Foe

senseFoeHere :: AntTest
senseFoeHere = senseFoe Here


senseFriend :: Direction -> AntTest
senseFriend d = sense d Friend

senseFriendHere :: AntTest
senseFriendHere = senseFriend Here



-- SPIRAL CRAZINESS

-- | Opening spiral, not covering all squares. A closed spiral is complicated. Ends with a turn
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



goSearchSpiral :: Condition -> AntImperative
goSearchSpiral c = goSearch (goSpiral 4) c Here


goSearchSpiral' :: AntTest -> AntImperative
goSearchSpiral' = doUntil (goSpiral 4)


goFFandBack :: AntImperative
goFFandBack = iList $
    [ goSearch safeMove Rock Ahead
    , turnAround
    , goSearch move Home Here ]


doFFandBack :: AntImperative -> AntImperative
doFFandBack s = iList $ 
    [ doSearch safeMove Rock Ahead s
    , turnAround
    , doSearch move Home Here s ]

