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


findWayHome :: AntImperative
findWayHome =
    iIfThenElse (TrySense Here (Marker P2))
    (
        (iIfThen (TrySense RightAhead (Marker P1)) turnAround)
        `iSeq`
        goSearch safeMove Home Here
    )
    findTrail


gatherFood :: AntImperative
gatherFood = findFood `iSeq` bringFoodHome


bringFoodHome :: AntImperative
bringFoodHome =
    iIfThenElse (TrySense Here Food)
        (pickup `iSeq` findWayHome `iSeq` dropFood)
        findFood


findFood :: AntImperative
findFood = goSearchSpiral Food


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
    , findFood'  -- find food
    , pickup
    , findMainOrHome  -- find main track
    , followTrackHome
    , dropAndStay ]  -- drop food at home or follow the main track

initMarkers g =
    iIfThenElse (TrySense LeftAhead Friend `Or` TrySense RightAhead Friend)
        (iTurnL `iSeq` g)
        markLoop


markLoop = iMark P1 `iSeq` markLine `iSeq` bounce `iSeq` markLine


markLine =
    iWhile (Not $ TrySense Ahead Rock)
        (iCase [ (marker P1, atMarker P1)
               , (marker P2, atMarker P2)
               , (marker P3, atMarker P3) ])
    where
        markerCase :: [(Pheromone, Pheromone -> AntImperative)] -> AntImperative
        markerCase l = iCase $ map (\(p, f) -> (marker p, f p)) l 
        atMarker p = safeMove `iSeq` iMark (pheromoneSucc p)

findFood' = --goSearchSpiral' (TrySense Here Food)
    doUntil (iList [goForwardNSteps 3, randomTurn]) (TrySense Here Food)


--goSearchSpiral' (TrySense Here Food)
findMainOrHome = doUntil (iList [randomTurn, move]) (home `Or` marker P1 `Or` marker P2 `Or` marker P3)

findDirToFollow p = doUntil iTurnR (markerAhead (pheromonePred p)) 
markerAhead p = TrySense Ahead (Marker p)


followTrackHome = iCase [ (marker P1, atMarker P1)
                              , (marker P2, atMarker P2)
                              , (marker P3, atMarker P3)]
    where atMarker p = findDirToFollow p `iSeq` followUntilHome 
pheromonePred P1 = P3 
pheromonePred P2 = P1 
pheromonePred P3 = P2 
pheromonePred _  = error "pheromonePred should always be called with {1,2,3}"
pheromoneSucc P1 = P2 
pheromoneSucc P2 = P3 
pheromoneSucc P3 = P1
pheromoneSucc _  = error "pheromoneSucc should always be called with {1,2,3}"


followUntilHome = doUntil followStep home 
    where followStep = moveOrWall turnUntilMark
          turnUntilMark = iCase [ (marker P1, findDirToFollow P1)
                                , (marker P2, findDirToFollow P2) 
                                , (marker P3, findDirToFollow P3) ] 


bounce = iCase [ (And wallLeft wallRight , random120)
               , (wallLeft               , turn60R)
               , (wallRight              , turn60L) ]
    where wallLeft  = TrySense LeftAhead Rock
          wallRight = TrySense RightAhead Rock
          random120 = chooseUniformly [ turn120L, turn120R ]


-- End of strategy

bounceUntil t = bounceUntilWhile t (iList [])
-- could use the case statement
bounceUntilWhile :: AntTest -> AntImperative -> AntImperative
bounceUntilWhile t ai = iWhile (Not $ t) $ 
                    doUntil (moveOrWall
                        (iIfThenElse (TrySense RightAhead Rock) 
                            (IfThenElse (TrySense LeftAhead Rock) turnAround iTurnL) 
                        (iTurnR)) `iSeq` ai) t

dropAndStay = iList [iDrop,
                     turnAround, 
                     findStay]
                        
findStay = iIfThenElse (Not $ TrySense LeftAhead Friend) (iTurnL `iSeq` move `iSeq` stay)
           $ iIfThenElse (Not $ TrySense RightAhead Friend) (iTurnR `iSeq` move `iSeq` stay)
           $ turnAround `iSeq`  
             (iIfThenElse (Not $ TrySense LeftAhead Friend) (iTurnL `iSeq` move `iSeq` stay)
            $ iIfThenElse (Not $ TrySense RightAhead Friend) (iTurnR `iSeq` move `iSeq` stay)
            $ iIfThen (Not $ TrySense Ahead Friend) (move `iSeq` stayUntil))

-- stay until another one is trying to stay near this location
stayUntil = iList [iWhile (Not $ TrySense Ahead Friend) (turnAround `iSeq` turnAround),
                    turnAround,
                    goForwardNSteps 5,
                    findFood']
stay = iWhile (Not $ TrySense Ahead FriendWithFood) iTurnL
-- End of strategy
