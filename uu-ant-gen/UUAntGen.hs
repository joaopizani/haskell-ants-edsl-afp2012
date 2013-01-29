module Main where

import Game.UUAntGen.AntAssembly
import Game.UUAntGen.AntDeepEmbedded;
import Game.UUAntGen.AntImperative;
import Game.UUAntGen.AntInstruction;
import Game.UUAntGen.AntMoves;
import Game.UUAntGen.AntTransformation;
import Game.UUAntGen.AntStrategies;
import System.Environment;

main = do  
    writeFile "../ants-sample/uu.ant" $ compile $ semanticsImp $ strategy'

