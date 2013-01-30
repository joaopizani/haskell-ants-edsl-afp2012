module Game.UUAntGen.Frontend.AntStrategies where

import Game.UUAntGen.Backend.AntAssembly
import Game.UUAntGen.Backend.AntDeepEmbedded
import Game.UUAntGen.Backend.AntInstruction

import Game.UUAntGen.Frontend.AntImperative
import Game.UUAntGen.Frontend.AntMoves



-- GENERAL-PURPOSE STRATEGIES, for the user

ricochet :: AntImperative
ricochet = ricochetWhile (iList [])

-- TODO How does this work? Does this work?
ricochetWhile ::  AntImperative -> AntImperative
ricochetWhile s = moveOrWall howToTurn `iSeq` s
    where
        rockL     = senseRock LeftAhead
        rockR     = senseRock RightAhead
        howToTurn = iCase [(rockL `And` rockR, turnAround), (rockL, turn60R), (rockR, turn60L)]


untilOverFood :: AntImperative -> AntImperative
untilOverFood st = doUntil st (And senseFoodHere (Not senseHomeHere))

untilOverHome :: AntImperative -> AntImperative
untilOverHome st = doUntil st senseHomeHere

findFoodSampleMap :: AntImperative
findFoodSampleMap = iList $
    [ iTurnR, iTurnR
    , safeGoFFUntil (TrySense Here Food)  --TODO really needs to be safe (from collisions)?
    , pickup, turnAround
    , safeGoFFUntil (TrySense Here Home)  --TODO really needs to be safe?
    , dropFood ]


bounceOnAny :: [Condition] -> AntImperative
bounceOnAny conds =
    iCase
        [ (left `And` right, random120)
        , (left            , turn120R)
        , (right           , turn120L)
        , (tautology       , random120) ]
    where
        left  = foldr1 Or $ map (TrySense LeftAhead) conds
        right = foldr1 Or $ map (TrySense RightAhead) conds
        random120 = chooseUniformly [turn120L, turn120R]


-- | Case-distinction when sensing for markers, using a different function to treat each case
markersCase :: Direction -> [(Pheromone, Pheromone -> AntImperative)] -> AntImperative
markersCase d ps = iCase $ map (\(p, f) -> (senseMarker d p, f p)) ps

-- | Case-distinction when sensing for markers here, using a different function to treat each case
markersHereCase :: [(Pheromone, Pheromone -> AntImperative)] -> AntImperative
markersHereCase = markersCase Here

-- | Senses given pheromones in order and applies the given (parameterized) strategy to the 1st match
withMarkers :: Direction -> [Pheromone] -> (Pheromone -> AntImperative) -> AntImperative
withMarkers d ps handler = markersCase d $ zip ps (repeat handler)

withMarkersHere :: [Pheromone] -> (Pheromone -> AntImperative) -> AntImperative
withMarkersHere = withMarkers Here


-- | Tests whether an ant is on top on any of the pheromones in the given list
isOverAnyOfMarks :: [Pheromone] -> AntTest
isOverAnyOfMarks ps = foldr1 Or (map senseMarkerHere ps)

-- | Perform some strategy until it comes over any of the pheromones in the given list
untilOverAnyOfMarks :: [Pheromone] -> AntImperative -> AntImperative
untilOverAnyOfMarks ps st = doUntil st (isOverAnyOfMarks ps)



-- | Aligns an ant (by turning the needed amount of times) to follow a certain pheromone
alignToFollow :: Pheromone -> AntImperative
alignToFollow p = doUntil iTurnR $ senseMarker Ahead p

-- | Regresses on an ordered track of pheromones until a condition is met
backOnTrackUntil :: [Pheromone] -> Condition -> AntImperative
backOnTrackUntil ps c = withMarkersHere ps step
    where step p = alignToFollow (pheromonePred p) `iSeq` (backOnTrackUntil' ps c)

backOnTrackUntil' :: [Pheromone] -> Condition -> AntImperative
backOnTrackUntil' ps c = doUntil (moveOrWall alignToPred) (TrySense Here c)
    where alignToPred = withMarkersHere ps (alignToFollow . pheromonePred)



-- TODO instead of only bouncing, another strategy would be nice: when meeting a pheromone track in
-- front of us, we "skip" that cell, so that ants driving on the road can "go over" the crossing.

-- | Paints a straight line with a cyclic sequence of pheromone markers, until meeting a wall. Also,
-- bounces whenever any of the conditions specified is met.  TODO (bouncing or skipping)
markLineBouncingOnAny :: [Condition] -> [Pheromone] -> AntImperative
markLineBouncingOnAny cs ps = doUntil (withMarkersHere ps step) (senseRock Ahead)
    where
        ps'    = map Marker ps
        cross  = foldr1 Or $ map (TrySense Ahead) cs
        step p = iIfThen cross (bounceOnAny ps') `iSeq` safeMove `iSeq` iMark (pheromoneSucc p)


-- | Paints a straight cyclic sequence of pheromone markers, until a wall. Bounces on itself
markLine :: [Pheromone] -> AntImperative
markLine ps = markLineBouncingOnAny (map Marker ps) ps








-- OUR STRATEGY #1

highwayPs, foodRoadPs :: [Pheromone]
highwayPs = [P0, P1, P2]
foodRoadPs = [P3, P4, P5]

-- | The predecessor of a pheromone. In this particular strategy, pheromones are in two cyclic
-- groups of 3 elements each: highway pheromones and food road (local) pheromones
pheromonePred :: Pheromone -> Pheromone
pheromonePred P2 = P1  -- highway pheromones
pheromonePred P1 = P0
pheromonePred P0 = P2
pheromonePred P5 = P4  -- food road pheromones
pheromonePred P4 = P3
pheromonePred P3 = P5

pheromoneSucc :: Pheromone -> Pheromone
pheromoneSucc P0 = P1  -- highway pheromones
pheromoneSucc P1 = P2
pheromoneSucc P2 = P0
pheromoneSucc P3 = P4  -- food road pheromones
pheromoneSucc P4 = P5
pheromoneSucc P5 = P3


-- | THE TOP LEVEL STRATEGY
strategy' :: AntImperative
strategy' = iList $
    [ iterate initMarkers iEmpty !! 6  -- code for the corner guys
    , chooseUniformly [gatherFood, protectLine]  -- protect main line or only gather food
    ]


gatherFood :: AntImperative
gatherFood = iForever $ iList
    [ untilOverFood ricochet  -- should it be ricochet?
    , pickup
    , untilOverAnyOfMarks highwayPs ricochet
    , backOnTrackUntil highwayPs Home
    , dropAndStay
    ]


initMarkers :: AntImperative -> AntImperative
initMarkers g =
    iIfThenElse (TrySense LeftAhead Friend `Or` TrySense RightAhead Friend)
        (iTurnL `iSeq` g)
        (markHighway `iSeq` gatherFood)


-- | Bounces only once on a wall
markHighway :: AntImperative
markHighway = iMark (head highwayPs) `iSeq` line `iSeq` bounceOnAny [Rock] `iSeq` line
    where line = markLine highwayPs


-- | Bounces always on walls, stops when it finds a highway
markFoodRoad :: AntImperative
markFoodRoad = iMark (head foodRoadPs) `iSeq` doUntil step (isOverAnyOfMarks highwayPs)
    where step = markLine foodRoadPs `iSeq` bounceOnAny [Rock]



dropAndStay :: AntImperative
dropAndStay = iList [dropFood, turnAround, findStay]


findStay :: AntImperative
findStay = iIfThen (TrySense Here (Marker P1)) $ 
            iIfThenElse (Not $ TrySense LeftAhead Friend) (iTurnL `iSeq` move `iSeq` stay)
           $ iIfThenElse (Not $ TrySense RightAhead Friend) (iTurnR `iSeq` move `iSeq` stay)
           $ turnAround `iSeq`  
             (iIfThenElse (Not $ TrySense LeftAhead Friend) (iTurnL `iSeq` move `iSeq` stay)
            $ iIfThenElse (Not $ TrySense RightAhead Friend) (iTurnR `iSeq` move `iSeq` stay)
            $ iIfThen (Not $ TrySense Ahead Friend) (move `iSeq` stayUntil))


-- stay until another one is trying to stay near this location
stayUntil :: AntImperative
stayUntil = iList $
    [ iWhile (Not $ senseFriend Ahead `And` senseMarker Ahead P1)
          (turn60L `iSeq` iMark P5)
    , turnAround
    , safeGoFFNSteps 3  --TODO really needs to be safe?
    , untilOverFood ricochet  --TODO should it be ricochet?
    ]


stay :: AntImperative
stay =
    iIfThen (Not $ senseMarkerHere P1) $
        iWhile (Not $ senseMarkerHere P2 `And` senseMarker Ahead P1)
            (turn60L `iSeq` iMark P5)


protectLine :: AntImperative
protectLine = iList $
    [ randomTurn
    , iWhile (senseFriend Ahead) turnAround
    , toEdgeOfHome
    , toLineStart
    , turn60R  --assumes we travel counterclockwise around the home
    , findStay
    , gatherFood ]


toEdgeOfHome :: AntImperative
toEdgeOfHome = iList $
    [ iWhile (senseHome Ahead) move
    , iWhile (Not $ senseHome LeftAhead) iTurnL
    ]


toLineStart :: AntImperative
toLineStart =
    iWhile (Not $ senseMarkerHere P1) $
        iIfThenElse (senseMarker Ahead P5 `And` senseFriend Ahead)
            (iList [turn60L, safeGoFFNSteps 2, turn60R, safeGoFFNSteps 2])  --TODO really needs to be safe?
            (iList [iIfThen (Not $ senseHome Ahead) turn60L, move])

-- End of OUR STRATEGY

-- Fallback simple strategy...

strategy'' :: AntImperative
strategy'' = iList $ [ findFood
                     , pickup
                     , turnAround
                     , returnToBase 
                     , iDrop ]
    where findFood      = untilOverFood tryMoveOrTurn
          returnToBase  = untilOverHome tryMoveOrTurn 
          tryMoveOrTurn = iIfThen (Not $ TryForward)
                                  randomTurn
 
