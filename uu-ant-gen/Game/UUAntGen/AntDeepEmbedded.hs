module Game.UUAntGen.AntDeepEmbedded where

import Game.UUAntGen.AntBasic 
import Game.UUAntGen.AntImperative 
import Game.UUAntGen.AntInstruction 

{-
goForwardNSteps :: Int -> [Action]
goForwardNSteps n = replicate n (Single CMove)

goFFUntilOrWall :: AntTest -> [Imperative] -> [Imperative]
goFFUntilOrWall t w = While (Negate t) [Single (CMoveOrWall w)]
-}

data Imperative 
    = Single Basic
    | Seq Imperative Imperative
    | IfThenElse AntTest Imperative Imperative 
    | IfThen AntTest Imperative
    | While AntTest Imperative 

data Basic
    = CMark Pheromone
    | CUnMark Pheromone
    | CDrop
    | CTurn Dexterity 
    deriving Show


semanticsBasic :: Basic -> AntStrategy
semanticsBasic (CMark p)       = aMark p
semanticsBasic (CUnMark p)     = aUnMark p
semanticsBasic CDrop           = aDrop
semanticsBasic (CTurn d)       = aTurn d

semanticsImperative :: Imperative -> AntStrategy
semanticsImperative (Single b)          = semanticsBasic b
semanticsImperative (Seq i1 i2)         = semanticsImperative i1 >>- 
                                          semanticsImperative i2
semanticsImperative (IfThenElse c t e)  = aIfThenElse c (semanticsImperative t)
                                                        (semanticsImperative e) 
semanticsImperative (IfThen c b)        = aIfThen c (semanticsImperative b)
semanticsImperative (While c b)         = aWhile c (semanticsImperative b)


