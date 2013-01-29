module Game.UUAntGen.AntStrategies where

import Game.UUAntGen.AntImperative
import Game.UUAntGen.AntMoves
import Game.UUAntGen.AntAssembly
import Game.UUAntGen.AntDeepEmbedded
import Game.UUAntGen.AntInstruction



-- BASIC STRATEGIES
markHome :: AntImperative
markHome = 
    (doSearch move Rock Ahead (iMark P1))
    `iSeq`
    iList
        [ turnAround, move, iTurnR, move, iTurnL
        , iWhile (notHome `And` p1Ahead) (iMark P2 `iSeq` safeMove)]
    where
        p1Ahead = TrySense LeftAhead (Marker P1)


findTrail :: AntImperative
findTrail = doUntil (goSpiral 2) (markerP2 `Or` home)
    where markerP2 = TrySense Here (Marker P2)

find :: AntImperative
find = ricochet --iList [goForwardNSteps 3, randomTurn]

findFood :: AntImperative
findFood = doUntil find food


findFoodSampleMap :: AntImperative
findFoodSampleMap = iList $
    [ iTurnR, iTurnR
    , goFFUntil (TrySense Here Food)
    , pickup, turnAround
    , goFFUntil (TrySense Here Home)
    , dropFood ]




-- A complete strategy (#1)

highwayPheromones, foodRoadPheromones :: [Pheromone]
highwayPheromones = [P0, P1, P2]
foodRoadPheromones = [P3, P4, P5]

strategy' :: AntImperative
strategy' = iList $
    [ iterate initMarkers iEmpty !! 6  -- code for the corner guys
    , chooseUniformly [gatherFood, 
            protectLine]]  -- protect the main line or if not necessary gather food

gatherFood :: AntImperative
gatherFood = iWhile (tautology) $ 
    iList [findFood  -- find food
    , pickup
    , findPheromoneTrack highwayPheromones
    , backOnPheromoneTrackUntil highwayPheromones Home
    , dropAndStay ]

initMarkers :: AntImperative -> AntImperative
initMarkers g =
    iIfThenElse (TrySense LeftAhead Friend `Or` TrySense RightAhead Friend)
        (iTurnL `iSeq` g)
        markHighway



bounce :: [Condition] -> AntImperative
bounce conds =
    iCase
        [ (And wallLeft wallRight , random120)
        , (wallLeft               , turn120R)
        , (wallRight              , turn120L)
        , (tautology              , random120) ]
    where
        wallLeft  = foldr1 Or $ map (TrySense LeftAhead) conds
        wallRight = foldr1 Or $ map (TrySense RightAhead) conds
        random120 = chooseUniformly [ turn120L, turn120R ]


pheromonePred :: Pheromone -> Pheromone
pheromonePred P2 = P1
pheromonePred P1 = P0
pheromonePred P0 = P2

pheromonePred P5 = P4
pheromonePred P4 = P3
pheromonePred P3 = P5


pheromoneSucc :: Pheromone -> Pheromone
pheromoneSucc P0 = P1
pheromoneSucc P1 = P2
pheromoneSucc P2 = P0

pheromoneSucc P3 = P4
pheromoneSucc P4 = P5
pheromoneSucc P5 = P3


markerCase :: [(Pheromone, Pheromone -> AntImperative)] -> AntImperative
markerCase l = iCase $ map (\(p, f) -> (marker p, f p)) l

withMarkers :: [Pheromone] -> (Pheromone -> AntImperative) -> AntImperative
withMarkers ps handler = markerCase $ zip ps (repeat handler)


isOnPheromoneTrack :: [Pheromone] -> AntTest
isOnPheromoneTrack ps = foldr1 Or (map marker ps)

findPheromoneTrack :: [Pheromone] -> AntImperative
findPheromoneTrack ps = doUntil find (isOnPheromoneTrack ps)


markLine :: [Pheromone] -> AntImperative
markLine ps = doUntil (withMarkers ps atMarker) (TrySense Ahead Rock)
    where
        allMarkers  = map Marker ps
        testcross   = foldr1 Or $ map (TrySense Ahead) allMarkers
        atMarker p  = (iIfThen testcross $ bounce allMarkers)
                      `iSeq` safeMove `iSeq` iMark (pheromoneSucc p)


markHighway :: AntImperative
markHighway = iMark (head highwayPheromones) `iSeq`
           line `iSeq` bounce [Rock] `iSeq` line `iSeq` gatherFood
    where line = markLine highwayPheromones


markFoodRoad :: AntImperative
markFoodRoad = iMark (head foodRoadPheromones) `iSeq`
               (doUntil loop $ isOnPheromoneTrack highwayPheromones)
    where
        line = markLine foodRoadPheromones
        loop = line `iSeq` bounce [Rock]



markerAhead :: Pheromone -> AntTest
markerAhead p = TrySense Ahead (Marker p)



-- | Follows a track of pheromones back to the nest
backOnPheromoneTrackUntil :: [Pheromone] -> Condition -> AntImperative
backOnPheromoneTrackUntil ps c = withMarkers ps atMarker
    where atMarker p = findDirToFollow p `iSeq` (followUntilCondition ps c)

findDirToFollow :: Pheromone -> AntImperative
findDirToFollow p = doUntil iTurnR (markerAhead (pheromonePred p)) 

followUntilCondition :: [Pheromone] -> Condition -> AntImperative
followUntilCondition ps c = doUntil followStep (TrySense Here c)
    where
        followStep = moveOrWall turnUntilMark
        turnUntilMark = withMarkers ps findDirToFollow

findHighway :: AntImperative
findHighway = findPheromoneTrack highwayPheromones


-- End of strategy



ricochet :: AntImperative
ricochet = ricochetWhile (iList [])
-- could use the case statement
ricochetWhile ::  AntImperative -> AntImperative
ricochetWhile ai = (moveOrWall
                        (iIfThenElse (TrySense RightAhead Rock) 
                            (IfThenElse (TrySense LeftAhead Rock) turnAround iTurnL) 
                        (iTurnR)) `iSeq` ai)

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
