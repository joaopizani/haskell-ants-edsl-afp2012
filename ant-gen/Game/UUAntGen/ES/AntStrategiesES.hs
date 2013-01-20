module AntStrategiesES where

import AntGenES

import Control.Monad.Supply

type AntStrategy = Supply StateID [AntInstruction] 

type StateID = Int

eS :: AntStrategy
eS = return $ singleton $ EI

aDrop_turn :: AntStrategy
aDrop_turn = aDrop >>- aTurn R

aDrop :: AntStrategy 
aDrop = do
    s <- supply
    return $ singleton $ Drop (AntState s)

aTurn :: Dexterity -> AntStrategy
aTurn d = do
    s <- supply
    return $ singleton $ Turn d (AntState s)

aTurnL :: AntStrategy
aTurnL = aTurn L

aTurnR :: AntStrategy
aTurnR = aTurn R

aSense :: Direction -> Condition -> AntStrategy
aSense d c = do 
    s <- supply
    return $ singleton $ Sense d ES (AntState s) c

aMove :: AntStrategy
aMove = do
    s <- supply
    return $ singleton $ Move ES (AntState s)

aMoveOrLeft :: AntStrategy
aMoveOrLeft = sqC aMove aTurnL eS

singleton = (:[])

infixr 6 >>-

(>>-) :: AntStrategy -> AntStrategy -> AntStrategy
s1 >>- s2 =
    do
       l1 <- s1
       l2 <- s2
       return $ ll l1 ++ l2 
        where ll l1 = case (last l1) of 
                    EI -> init l1
                    _  -> l1

sqC :: AntStrategy -> AntStrategy -> AntStrategy -> AntStrategy
sqC s1 s2 s3 =
    do
        l1 <- s1
        l2 <- s2
        l3 <- s3
        return $ (init l1
                    ++ [modifyState (const (AntState (length l2))) (last l1)])
                    ++ l2
                    ++ l3

modifyState f (Sense a b s c) = (Sense a b (f s) c)
modifyState f (Mark a s)      = (Mark a (f s))                   
modifyState f (UnMark a s)    = (UnMark a (f s))                 
modifyState f (PickUp s a)    = (PickUp (f s) a)                 
modifyState f (Drop s)        = (Drop (f s))                     
modifyState f (Turn a s)      = (Turn a (f s))                   
modifyState f (Move s a)      = (Move (f s) a)                   
modifyState f (Flip a b s)    = (Flip a b (f s))                 

replaceState s = modifyState (const s)

incrementState delta = modifyState (\(AntState s) -> AntState (s+delta)) 

getNextState :: AntInstruction -> AntState
getNextState (Sense _ _ d _) = d 
getNextState (Mark _ s)      = s
getNextState (UnMark _ s)    = s
getNextState (PickUp s _)    = s
getNextState (Drop s)        = s
getNextState (Turn _ s)      = s
getNextState (Move s _)      = s
getNextState (Flip _ _ s)    = s


