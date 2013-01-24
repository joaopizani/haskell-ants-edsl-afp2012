module Main where

import Game.UUAntGen.AntBasic;
import Game.UUAntGen.AntImperative;
import Game.UUAntGen.AntInstruction;
import Game.UUAntGen.AntMoves;
import Game.UUAntGen.AntTransformation;
import System.Environment;

main = do 
    [path] <- getArgs
    writeFile path $ printInstructionMap $ fromKeysToLineNumbers $ ghostBuster $ runAntStrategy $ strategy

strategy = goForwardNSteps 10
