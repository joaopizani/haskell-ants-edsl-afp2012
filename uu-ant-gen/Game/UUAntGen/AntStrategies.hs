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


markLoop = markLine
             -- Code for +1 repetition             
--           `iSeq` 
--           (iIfThenElse (TrySense LeftAhead Rock)
--                        (iIfThen (Not $ TrySense RightAhead Rock)
--                                 (iTurnR `iSeq` markLine)) 
--                        (iTurnL `iSeq` markLine)) 


-- FIXME: Maybe use safeMove?
markLine =
    iWhile (Not $ TrySense Ahead Rock)
        (iIfThenElse (Not $ TrySense LeftAhead Rock)
            (iList [iTurnL, move, iMark P4, turn120R, move, iTurnL, iMark P5])
            (move `iSeq` iMark P5)
        )


findFood' = --goSearchSpiral' (TrySense Here Food)
    doUntil (iList [goForwardNSteps 3, randomTurn]) (TrySense Here Food)


--goSearchSpiral' (TrySense Here Food)
findMainOrHome = doUntil (iList [randomTurn, move]) (home `Or` marker P5 `Or` marker P4)


dropFoodOrFollowTrack = iCase [(home, atHome), (marker P4, atMarker P4), (marker P5, atMarker P5)]
    where atHome = iDrop --FIXME
          atMarker P4 = doUntil iTurnR (TrySense Ahead (Marker P4)) `iSeq` goUntilHome
          atMarker P5 = doUntil iTurnL (TrySense Ahead (Marker P5)) `iSeq` goUntilHome
          goUntilHome = doUntil move home
-- End of strategy

