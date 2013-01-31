module Main where

import Game.UUAntGen.Backend.AntDeepEmbedded
import Game.UUAntGen.Backend.AntInstruction
import Game.UUAntGen.Backend.AntTransformation

import Game.UUAntGen.Frontend.AntImperative
import Game.UUAntGen.Frontend.AntMoves
import Game.UUAntGen.Frontend.AntStrategies


main :: IO ()
main = putStr $ compileAndPrint (head strategies)



-- LIST OF STRATEGIES
strategies :: [AntImperative]
strategies = 
    [
      winnerStrategy2
    , winnerStrategy1
    , fallbackStrategy
    ]

-- THE TOP LEVEL STRATEGIES
winnerStrategy1 :: AntImperative
winnerStrategy1 = iList $
    [ highwayDetectAndBuild  -- if on a corner, go build highway
    , iDelay 800  -- wait some turns to give the highway guys a headstart
    , disperse
    , randomTurn  -- Divert in random directions, avoid a big group together
    , gatherFood sillyRandomStep -- then gather food
    ]


winnerStrategy2 :: AntImperative
winnerStrategy2 = iList $
    [ highwayDetectAndBuild  -- if on a corner, go build highway
    , iDelay 800  -- wait some turns to give the highway guys a headstart
    , disperse
    , randomTurn  -- Divert in random directions, avoid a big group together
    , gatherFood ricochet  -- then gather food
    ]


fallbackStrategy :: AntImperative
fallbackStrategy = iList $ 
    [ findFood
    , pickup
    , turnAround
    , returnToBase
    , iDrop
    ]
    where
        findFood      = untilOverFood tryMoveOrTurn
        returnToBase  = untilOverHome tryMoveOrTurn 
        tryMoveOrTurn = iIfThen (Not $ TryForward) randomTurn




-- COMPILATION PIPELINE: From AntImperative and/or AntStrategy to String (ant file)
translateToIL :: AntImperative -> AntStrategy'
translateToIL = runAntStrategy . semanticsImp

transformIL :: AntStrategy' -> IMap
transformIL = keysToLineNumbers . ghostBuster . topLevelForever


-- | TOP LEVEL compile function. Performs all program transformations and optimizations
compile :: AntImperative -> IMap
compile = transformIL . translateToIL


-- | TOP LEVEL compiles and prints an ant file
compileAndPrint :: AntImperative -> String
compileAndPrint = printInstructions . compile

-- Helper function, compiles and prints, but also with the keys of the map (for debugging purposes)
compileAndPrintWithKeys :: AntImperative -> String
compileAndPrintWithKeys = printInstructionsWithKeys . compile

