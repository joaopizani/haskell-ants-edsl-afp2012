module Main where

import Game.UUAntGen.AntBasic;
import Game.UUAntGen.AntImperative;
import Game.UUAntGen.AntInstruction;
import Game.UUAntGen.AntMoves;
import Game.UUAntGen.AntTransformation;
import System.Environment;

main = do  
    writeFile "../ants-sample/uu.ant" $ compile $ strategy

strategy = randomTurn >>- doFFandBack aDrop 
