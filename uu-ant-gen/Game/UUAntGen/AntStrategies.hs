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
    , dropFoodOrFollowTrack ]  -- drop food at home or follow the main track
    -- FIXME: remove this
    ++ replicate 1000 iDrop -- prevent code from looping, for a while...


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
        atMarker p = safeMove `iSeq` iMark (pheromonePred p)

findFood' = --goSearchSpiral' (TrySense Here Food)
    doUntil (iList [goForwardNSteps 3, randomTurn]) (TrySense Here Food)


--goSearchSpiral' (TrySense Here Food)
findMainOrHome = doUntil (iList [randomTurn, move]) (home `Or` marker P5 `Or` marker P4)


findDirToFollow p = doUntil iTurnR (markerAhead (pred p)) 
markerAhead p = TrySense Ahead (Marker p)


dropFoodOrFollowTrack = iCase [ (home, atHome)
                              , (marker P1, atMarker P1)
                              , (marker P2, atMarker P2)
                              , (marker P3, atMarker P3)]
    where atHome = iDrop --FIXME
          atMarker p = findDirToFollow p `iSeq` followUntilHome

pheromonePred P1 = P3 
pheromonePred P2 = P1 
pheromonePred P3 = P2 
pheromonePred _  = error "pheromonePred should always be called with {1,2,3}"

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

