module Game.UUAntGen.Frontend.AntStrategies where

import Game.UUAntGen.Backend.AntAssembly
import Game.UUAntGen.Backend.AntDeepEmbedded
import Game.UUAntGen.Backend.AntInstruction

import Game.UUAntGen.Frontend.AntImperative
import Game.UUAntGen.Frontend.AntMoves



-- BASIC STRATEGIES
markHome :: AntImperative
markHome = 
    (doSearch move Rock Ahead (iMark P1))
    `iSeq`
    iList
        [ turnAround, move, iTurnR, move, iTurnL
        , iWhile ((Not senseHomeHere) `And` p1Ahead) (iMark P2 `iSeq` safeMove)]
    where
        p1Ahead = TrySense LeftAhead (Marker P1)


findTrail :: AntImperative
findTrail = doUntil (goSpiral 2) (markerP2 `Or` senseHomeHere)
    where markerP2 = TrySense Here (Marker P2)


find :: AntImperative
find = ricochet

ricochet :: AntImperative
ricochet = ricochetWhile (iList [])
-- could use the case statement
ricochetWhile ::  AntImperative -> AntImperative
ricochetWhile ai = (moveOrWall
                        (iIfThenElse (TrySense RightAhead Rock) 
                            (IfThenElse (TrySense LeftAhead Rock) turnAround iTurnL) 
                        (iTurnR)) `iSeq` ai)


findFood :: AntImperative
findFood = doUntil find senseFoodHere


findFoodSampleMap :: AntImperative
findFoodSampleMap = iList $
    [ iTurnR, iTurnR
    , goFFUntil (TrySense Here Food)
    , pickup, turnAround
    , goFFUntil (TrySense Here Home)
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





-- A complete strategy (#1)

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
    [ findFood  -- find food
    , pickup
    , findPheromoneTrack highwayPs
    , backOnPheromoneTrackUntil highwayPs Home
    , dropAndStay
    ]


initMarkers :: AntImperative -> AntImperative
initMarkers g =
    iIfThenElse (TrySense LeftAhead Friend `Or` TrySense RightAhead Friend)
        (iTurnL `iSeq` g)
        (markHighway `iSeq` gatherFood)






isOnPheromoneTrack :: [Pheromone] -> AntTest
isOnPheromoneTrack ps = foldr1 Or (map senseMarkerHere ps)

findPheromoneTrack :: [Pheromone] -> AntImperative
findPheromoneTrack ps = doUntil find (isOnPheromoneTrack ps)


-- TODO parameterize on crossing test: markLine = markBouncingOnMeet tautology
-- TODO parameterize on stopping condition
markLine :: [Pheromone] -> AntImperative
markLine ps = doUntil (withMarkersHere ps step) (senseRock Ahead)
    where
        ps'    = map Marker ps
        cross  = foldr1 Or $ map (TrySense Ahead) ps'
        step p = (iIfThen cross $ bounceOnAny ps') `iSeq` safeMove `iSeq` iMark (pheromoneSucc p)


markHighway :: AntImperative
markHighway = iMark (head highwayPs) `iSeq` line `iSeq` bounceOnAny [Rock] `iSeq` line
    where line = markLine highwayPs


markFoodRoad :: AntImperative
markFoodRoad = iMark (head foodRoadPs) `iSeq` doUntil step (isOnPheromoneTrack highwayPs)
    where step = markLine foodRoadPs `iSeq` bounceOnAny [Rock]




-- | Follows a track of pheromones back to the nest
backOnPheromoneTrackUntil :: [Pheromone] -> Condition -> AntImperative
backOnPheromoneTrackUntil ps c = withMarkersHere ps atMarker
    where atMarker p = findDirToFollow p `iSeq` (followUntilCondition ps c)

findDirToFollow :: Pheromone -> AntImperative
findDirToFollow p = doUntil iTurnR $ senseMarker Ahead (pheromonePred p)

followUntilCondition :: [Pheromone] -> Condition -> AntImperative
followUntilCondition ps c = doUntil followStep (TrySense Here c)
    where
        followStep = moveOrWall turnUntilMark
        turnUntilMark = withMarkersHere ps findDirToFollow

findHighway :: AntImperative
findHighway = findPheromoneTrack highwayPs


-- End of strategy




dropAndStay :: AntImperative
dropAndStay = iList [iDrop,
                     turnAround, 
                     findStay]


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
stayUntil = iList [iWhile (Not $ (And (TrySense Ahead Friend) (TrySense Ahead (Marker P1)))) 
                        (iTurnL `iSeq` iMark P5),
                    turnAround,
                    goForwardNSteps 3,
                    findFood]

stay :: AntImperative
stay = iIfThen (Not $ TrySense Here (Marker P1)) $ 
        iWhile 
        (Not $ (And (TrySense Here (Marker P2)) (TrySense Ahead (Marker P1)))) 
            (iTurnL `iSeq` iMark P5)

protectLine :: AntImperative
protectLine = iList [randomTurn,
                     iWhile (TrySense Ahead Friend) turnAround,
                     toEdgeOfHome,
                     toLineStart,
                     iTurnR, --assumes we travel counterclockwise around the home
                     findStay,
                     gatherFood]

toEdgeOfHome :: AntImperative
toEdgeOfHome = iList [iWhile (TrySense Ahead Home) move,
                        iWhile (Not $ TrySense LeftAhead Home) iTurnL]

toLineStart :: AntImperative
toLineStart = iWhile (Not $ (TrySense Here (Marker P1))) $ 
                iIfThenElse (And (TrySense Ahead (Marker P5)) (TrySense Ahead Friend))
                    (iList [iTurnL, goForwardNSteps 2, iTurnR, goForwardNSteps 2])
                    (iList [iIfThen (Not $ TrySense Ahead Home) iTurnL, move])

-- End of strategy
