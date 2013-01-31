module Game.UUAntGen.Frontend.AntStrategies where

import Game.UUAntGen.Backend.AntAssembly
import Game.UUAntGen.Backend.AntDeepEmbedded
import Game.UUAntGen.Backend.AntInstruction

import Game.UUAntGen.Frontend.AntImperative
import Game.UUAntGen.Frontend.AntMoves



-- GENERAL-PURPOSE STRATEGIES, for the user

-- | Try to do a safe move, use disperse as a last resort (non-recoverable)
safeMoveDisperse :: AntImperative
safeMoveDisperse = safeMoveLR disperse


-- | Performs a certain search step until we are over food (OUTSIDE the home nest)
untilOverFood :: AntImperative -> AntImperative
untilOverFood st = doUntil st (senseFoodHere `And` (Not senseHomeHere))

-- | Performs a certain search step until we are over our own home nest
untilOverHome :: AntImperative -> AntImperative
untilOverHome st = doUntil st senseHomeHere


-- | Considering that we face an obstacle, bounce like a ray of light according to our angle
bounceOnAny :: [Condition] -> AntImperative
bounceOnAny conds = iCase [(l `And` r, rand), (l, turn120R), (r, turn120L), (tautology, rand)]
    where
        l    = foldr1 Or $ map (TrySense LeftAhead) conds
        r    = foldr1 Or $ map (TrySense RightAhead) conds
        rand = chooseUniformly [turn120L, turn120R]


-- | A step of a ricocheting movement. Tries always to go forward and bounce on walls
ricochet :: AntImperative
ricochet = handleWall `iSeq` safeMoveDisperse
    where handleWall = iIfThen (senseRock Ahead) (bounceOnAny [Rock])


-- | Moves forward with probability n/(n+1), then makes a random "soft" turn (60L or 60R)
sillyRandomStepSized :: Int -> AntImperative
sillyRandomStepSized n = doWithChance n move `iSeq` chooseUniformly [turn60L, turn60R]

-- | Moves forward with probability 0,999 then makes a random "soft" turn (60L or 60R)
sillyRandomStep :: AntImperative
sillyRandomStep = sillyRandomStepSized 1000


-- | A sequence of 10 randomized movements. Can be used to "reset" any search route and try
-- to escape deadlocks and livelocks
disperse :: AntImperative
disperse = iList (replicate 10 sillyRandomStep)



-- | Case-distinction when sensing for markers, using a different function to treat each case
markersCase :: Direction -> [(Pheromone, Pheromone -> AntImperative)] -> AntImperative
markersCase d ps = iCase $ map (\(p, f) -> (senseMarker d p, f p)) ps

-- | Case-distinction when sensing for markers here, using a different function to treat each case
markersHereCase :: [(Pheromone, Pheromone -> AntImperative)] -> AntImperative
markersHereCase = markersCase Here

-- | Senses given pheromones in order and applies the given (parameterized) strategy to the 1st match
withMarkers :: Direction -> [Pheromone] -> (Pheromone -> AntImperative) -> AntImperative
withMarkers d ps handler = markersCase d $ zip ps (repeat handler)

-- | Senses given pheromones in order and applies the given (parameterized) strategy to the 1st match
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
-- bounces whenever any of the conditions specified is met.
markLineBouncingOnAny :: [Condition] -> [Pheromone] -> AntImperative
markLineBouncingOnAny cs ps = doUntil (withMarkersHere ps step) (senseRock Ahead)
    where
        ps'    = map Marker ps
        cross  = foldr1 Or $ map (TrySense Ahead) cs
        step p = iIfThen cross (bounceOnAny ps') `iSeq` safeMove `iSeq` iMark (pheromoneSucc p)

-- | Paints a straight cyclic sequence of pheromone markers, until a wall. Bounces on itself
markLine :: [Pheromone] -> AntImperative
markLine ps = markLineBouncingOnAny (map Marker ps) ps


findFoodSampleMap :: AntImperative
findFoodSampleMap = iList $
    [ iTurnR, iTurnR
    , safeGoFFUntil senseFoodHere
    , pickup, turnAround
    , safeGoFFUntil senseHomeHere
    , dropFood
    ]





-- OUR STRATEGY BLOCK

-- | We have "highways" and local roads in our strategies. A highway connects the nest to the
-- rest of the map, while a local road connects a source of food to a highway (or the nest). Each
-- of these paths are a mutually-exclusive cyclic sequence of pheromones
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


-- | Bounces only once on a wall
markHighway :: AntImperative
markHighway = iMark (head highwayPs) `iSeq` line `iSeq` bounceOnAny [Rock] `iSeq` line
    where line = markLine highwayPs

-- | Bounces always on walls, stops when it finds a highway
markFoodRoad :: AntImperative
markFoodRoad = iMark (head foodRoadPs) `iSeq` doUntil step (isOverAnyOfMarks highwayPs)
    where step = markLine foodRoadPs `iSeq` bounceOnAny [Rock]


-- | Turns at most 6 times around, while testing the position of friends around.
-- This is to detect whether we are in a corner. If we are, then go build highway.
highwayDetectAndBuild :: AntImperative
highwayDetectAndBuild = repeatedIfThen' turn60L 6 noFriendsOnSides markHighway
    where noFriendsOnSides = Not (senseFriend LeftAhead `Or` senseFriend RightAhead)


-- | The most important block that an ant executes during her life, in an infinite loop.
gatherFood :: AntImperative -> AntImperative
gatherFood step = iForever $ iList
    [ untilOverFood step
    , pickup
    , untilOverAnyOfMarks highwayPs step
    , backOnTrackUntil highwayPs Home
    , dropAndStay
    , disperse
    ]



dropAndStay :: AntImperative
dropAndStay = iList [dropFood, turnAround, findStay]


findStay :: AntImperative
findStay =
    iIfThen (senseMarkerHere P0) $
        iCase $
            [ (noFriendLA, left)
            , (noFriendRA, right)
            , (tautology, turnAround `iSeq` (iCase
                  [ (noFriendLA, left)
                  , (noFriendRA, right)
                  , (noFriendAh, move `iSeq` stayUntil)
                  , (tautology, iEmpty) 
                        -- iList [iWhile (senseFriend Ahead) iTurnL
                          --      , goFFNSteps 5
                             --   , chooseUniformly [iTurnR, iTurnL]])
                  ]))
            ]
    where
        noFriendLA = Not (senseFriend LeftAhead)
        noFriendRA = Not (senseFriend RightAhead)
        noFriendAh = Not (senseFriend Ahead)
        left  = turn60L `iSeq` move `iSeq` stay
        right = turn60R `iSeq` move `iSeq` stay


-- stay until another one is trying to stay near this location
stayUntil :: AntImperative
stayUntil = iList $
    [ iWhile (Not $ senseFriend Ahead `And` senseMarker Ahead P0)
          (turn60L `iSeq` iMark P5)
    , turnAround
    , safeGoFFNSteps 3
    , untilOverFood ricochet
    ]


stay :: AntImperative
stay =
    iIfThen (Not $ senseMarkerHere P1) $
        iWhile (Not $ senseMarkerHere P2 `And` senseMarker Ahead P1)
            (turn60L `iSeq` iMark P5)


protectLine :: AntImperative
protectLine = iIfThen (senseHome Here) $ iList $
    [ randomTurn
    , iWhile (senseFriend Ahead) turnAround
    , toEdgeOfHome
    , toLineStart
    , turn60R  --assumes we travel counterclockwise around the home
    , findStay
    , gatherFood ricochet ]


toEdgeOfHome :: AntImperative
toEdgeOfHome = iList $
    [ iWhile (senseHome Ahead) move
    , iWhile (Not $ senseHome LeftAhead) iTurnL
    ]


toLineStart :: AntImperative
toLineStart =
    iWhile (Not $ senseMarkerHere P0) $
        iIfThenElse (senseMarker Ahead P5 `And` senseFriend Ahead)
            (iList [turn60L, safeGoFFNSteps 2, turn60R, safeGoFFNSteps 2])
            (iList [iIfThen (Not $ senseHome Ahead) turn60L, move])

