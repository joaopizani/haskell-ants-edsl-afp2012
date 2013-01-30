module Main where

import Game.UUAntGen.AntDeepEmbedded;
import Game.UUAntGen.AntImperative;
import Game.UUAntGen.AntInstruction;
import Game.UUAntGen.AntTransformation;
import Game.UUAntGen.AntStrategies;


main :: IO ()
main = writeFile "../ants-sample/uu.ant" $ compileAndPrint $ strategy'


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

