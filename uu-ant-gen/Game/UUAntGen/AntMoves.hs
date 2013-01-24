module Game.UUAntGen.AntMoves where

import Game.UUAntGen.AntBasic
import Game.UUAntGen.AntImperative
import Game.UUAntGen.AntInstruction

import qualified Data.Map as M
import Control.Monad.Supply (supply)

aMove :: AntStrategy
aMove = do 
    idx <- supply
    gidx <- supply
    let ghost = (gidx, Ghost gidx idx idx)
        allInstrs = M.fromList [ghost, (idx, Move gidx gidx)]
    return $ AntStrategy' allInstrs idx gidx 


goFFUntilOrRock :: AntTest -> AntStrategy -> AntStrategy
goFFUntilOrRock t sw = aWhile (Negation t) (aMoveOrWall sw) 

goFFUntil :: AntTest-> AntStrategy
goFFUntil t = aWhile (Negation t) aMove

goForwardNSteps :: Int -> AntStrategy
goForwardNSteps n = foldr (>>-) aMove $ replicate (n-1) aMove

