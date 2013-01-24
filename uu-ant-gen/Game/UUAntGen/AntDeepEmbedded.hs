module Game.UUAntGen.AntDeepEmbedded where

import Game.UUAntGen.AntBasic 
import Game.UUAntGen.AntImperative 
import Game.UUAntGen.AntInstruction 

import Data.List (intersperse)

withPheromone :: Pheromone -> Imperative -> Imperative
withPheromone p (Single r)         = Single r
withPheromone p (IfThenElse c t f) = IfThenElse c (withPheromone p t) 
                                                  (withPheromone p f)
withPheromone p (IfThen c b)       = IfThen c (withPheromone p b) 
withPheromone p (While c b)        = While c (withPheromone p b) 
withPheromone p (SideEffect i)     = SideEffect i
withPheromone p (IList l)          = IList $ intersperse (Single (CMark p)) l

goForwardNSteps :: Int -> Imperative 
goForwardNSteps n = IList (replicate n aMove)

goFFUntil :: AntTest -> Imperative 
goFFUntil t = While (Not t) aMove

goFFUntilOrWall :: AntTest -> Imperative -> Imperative
goFFUntilOrWall t w = While (Not t) (aMoveOrWall w) 

aMove :: Imperative
aMove = SideEffect TryForward 

aMoveOrWall :: Imperative -> Imperative
aMoveOrWall wi = IfThen (Not TryForward) wi

data Imperative 
    = Single Basic
    | IfThenElse AntTest Imperative Imperative 
    | IfThen AntTest Imperative
    | While AntTest Imperative 
    | SideEffect AntTest
    | IList [Imperative]


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
semanticsImperative (IfThenElse c t e)  = aIfThenElse c (semanticsImperative t)
                                                        (semanticsImperative e) 
semanticsImperative (IfThen c b)        = aIfThen c (semanticsImperative b)
semanticsImperative (While c b)         = aWhile c (semanticsImperative b)
semanticsImperative (SideEffect t)      = aTest t 
semanticsImperative (IList l)           = semanticsImperativeList l
    where semanticsImperativeList (x:[]) = semanticsImperative x
          semanticsImperativeList (x:xs) = semanticsImperative x >>- 
                                           semanticsImperativeList xs  

