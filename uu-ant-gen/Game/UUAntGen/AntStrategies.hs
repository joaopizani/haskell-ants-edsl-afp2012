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
strategy' = iList $
    [ iterate initMarkers iEmpty !! 6  -- code for the corner guys
    , chooseUniformly [gatherFood, 
            protectLine]]  -- protect the main line or if not necessary gather food

gatherFood = iWhile (tautology) $ 
    iList [findFood  -- find food
    , pickup
    , findMain  -- find main track
    , followTrackHome
    , dropAndStay ]

initMarkers g =
    iIfThenElse (TrySense LeftAhead Friend `Or` TrySense RightAhead Friend)
        (iTurnL `iSeq` g)
        markLoop



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



pheromonePred P1 = P3
pheromonePred P2 = P1
pheromonePred P3 = P2
pheromonePred _  = error "pheromonePred should always be called with {1,2,3}"

pheromoneSucc P1 = P2
pheromoneSucc P2 = P3
pheromoneSucc P3 = P1
pheromoneSucc _  = error "pheromoneSucc should always be called with {1,2,3}"


markLoop = iMark P1 `iSeq` markLine `iSeq` bounce [Rock] `iSeq` markLine `iSeq` gatherFood

markLine =
    iWhile (Not $ TrySense Ahead Rock )
        (iCase [ (marker P1, atMarker P1)
               , (marker P2, atMarker P2)
               , (marker P3, atMarker P3) ])
    where
        markerCase :: [(Pheromone, Pheromone -> AntImperative)] -> AntImperative
        markerCase l = iCase $ map (\(p, f) -> (marker p, f p)) l 
        atMarker p = (iIfThen testHighway $ bounce allMarkers)
                     `iSeq` safeMove `iSeq` iMark (pheromoneSucc p)
        allMarkers  = map Marker [P1, P2, P3]
        testHighway = foldr1 Or $ map (TrySense Ahead) allMarkers



--goSearchSpiral' (TrySense Here Food)
findMain = doUntil find (marker P1 `Or` marker P2 `Or` marker P3)

findDirToFollow p = doUntil iTurnR (markerAhead (pheromonePred p)) 
markerAhead p = TrySense Ahead (Marker p)


followTrackHome = iCase [ (marker P1, atMarker P1)
                              , (marker P2, atMarker P2)
                              , (marker P3, atMarker P3)]
    where
        atMarker p = findDirToFollow p `iSeq` followUntilHome




followUntilHome = doUntil followStep home 
    where followStep = moveOrWall turnUntilMark
          turnUntilMark = iCase [ (marker P1, findDirToFollow P1)
                                , (marker P2, findDirToFollow P2) 
                                , (marker P3, findDirToFollow P3) ] 


-- End of strategy

ricochet = ricochetWhile (iList [])
-- could use the case statement
ricochetWhile ::  AntImperative -> AntImperative
ricochetWhile ai = (moveOrWall
                        (iIfThenElse (TrySense RightAhead Rock) 
                            (IfThenElse (TrySense LeftAhead Rock) turnAround iTurnL) 
                        (iTurnR)) `iSeq` ai)

dropAndStay = iList [iDrop,
                     turnAround, 
                     findStay]


findStay = iIfThen (TrySense Here (Marker P1)) $ 
            iIfThenElse (Not $ TrySense LeftAhead Friend) (iTurnL `iSeq` move `iSeq` stay)
           $ iIfThenElse (Not $ TrySense RightAhead Friend) (iTurnR `iSeq` move `iSeq` stay)
           $ turnAround `iSeq`  
             (iIfThenElse (Not $ TrySense LeftAhead Friend) (iTurnL `iSeq` move `iSeq` stay)
            $ iIfThenElse (Not $ TrySense RightAhead Friend) (iTurnR `iSeq` move `iSeq` stay)
            $ iIfThen (Not $ TrySense Ahead Friend) (move `iSeq` stayUntil))


-- stay until another one is trying to stay near this location
stayUntil = iList [iWhile (Not $ (And (TrySense Ahead Friend) (TrySense Ahead (Marker P1)))) 
                        (iTurnL `iSeq` iMark P5),
                    turnAround,
                    goForwardNSteps 3,
                    findFood]

stay = iIfThen (Not $ TrySense Here (Marker P1)) $ 
        iWhile 
        (Not $ (And (TrySense Here (Marker P2)) (TrySense Ahead (Marker P1)))) 
            (iTurnL `iSeq` iMark P5)

protectLine = iList [randomTurn,
                     iWhile (TrySense Ahead Friend) turnAround,
                     toEdgeOfHome,
                     toLineStart,
                     iTurnR, --assumes we travel counterclockwise around the home
                     findStay,
                     gatherFood]

toEdgeOfHome = iList [iWhile (TrySense Ahead Home) move,
                        iWhile (Not $ TrySense LeftAhead Home) iTurnL]

toLineStart = iWhile (Not $ (TrySense Here (Marker P1))) $ 
                iIfThenElse (And (TrySense Ahead (Marker P5)) (TrySense Ahead Friend))
                    (iList [iTurnL, goForwardNSteps 2, iTurnR, goForwardNSteps 2])
                    (iList [iIfThen (Not $ TrySense Ahead Home) iTurnL, move])

-- End of strategy
