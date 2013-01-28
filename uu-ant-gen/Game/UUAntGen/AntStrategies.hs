module Game.UUAntGen.AntStrategies where

import Game.UUAntGen.AntImperative
import Game.UUAntGen.AntMoves
import Game.UUAntGen.AntAssembly
import Game.UUAntGen.AntDeepEmbedded
import Game.UUAntGen.AntInstruction

-- A strategy
strategy' = iList $ [ iterate initMarkers iEmpty !! 6 -- code for the corner guys
                    , findFood'                       -- find food
                    , pickup                          
                    , findMainOrHome                  -- find main track
                    , dropFoodOrFollowTrack           -- drop food at home or follow the
                                                      -- main track
                    ] 
                  -- FIXME: remove this
                  ++ replicate 1000 iDrop -- prevent code from looping, for a while...

initMarkers g = iIfThenElse (TrySense LeftAhead Friend `Or`  
                             TrySense RightAhead Friend)
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
markLine = iWhile (Not $ TrySense Ahead Rock) 
                  (iIfThenElse (Not $ TrySense LeftAhead Rock)
                               (iList $ [ iTurnL, move, iMark P4, turn120R
                                        , move , iTurnL, iMark P5 ])
                               (move `iSeq` iMark P5))

findFood' = --goSearchSpiral' (TrySense Here Food)
            doUntil (iList $ [move,move,move,randomTurn]) 
                    (TrySense Here Food) 

findMainOrHome = --goSearchSpiral' (TrySense Here Food)
                 doUntil (iList $ [randomTurn,move])
                         (TrySense Here Home `Or` 
                          TrySense Here (Marker P5) `Or` 
                          TrySense Here (Marker P4)) 

dropFoodOrFollowTrack = iCase [ (TrySense Here Home, atHome)
                              , (TrySense Here (Marker P4), atMarker P4)
                              , (TrySense Here (Marker P5), atMarker P5) ]
    where atHome = iDrop --FIXME
          atMarker P4 = doUntil iTurnR (TrySense Ahead (Marker P4)) `iSeq` goUntilHome
          atMarker P5 = doUntil iTurnL (TrySense Ahead (Marker P5)) `iSeq` goUntilHome
          goUntilHome = doUntil move (TrySense Here Home)  
                                    
      
-- End of strategy                       


