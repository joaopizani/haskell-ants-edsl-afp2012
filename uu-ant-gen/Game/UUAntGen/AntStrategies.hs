module Game.UUAntGen.AntStrategies where

import Game.UUAntGen.AntAssembly
import Control.Monad.State


type AntStrategy = State StateID [AntInstruction] 

type StateID = Int

aDrop_turn :: AntStrategy
aDrop_turn = aDrop >>- aTurn R

aDrop :: AntStrategy 
aDrop = do 
    modify (+1)
    s <- get
    return $ singleton $ Drop (AntState s)

aTurn :: Dexterity -> AntStrategy
aTurn d = do
    modify (+1)
    s <- get
    return $ singleton $ Turn d (AntState s)

aTurnL :: AntStrategy
aTurnL = aTurn L

aTurnR :: AntStrategy
aTurnR = aTurn R

aSense :: Direction -> Condition -> AntStrategy
aSense d c = do
    modify (+1)
    s <- get
    return $ singleton $ Sense d (AntState s) (AntState s) c

aMove :: AntStrategy
aMove = do
    modify (+1)
    s <- get
    return $ singleton $ Move (AntState s) (AntState s)

singleton = (:[])

infixr 6 >>-

(>>-) :: AntStrategy -> AntStrategy -> AntStrategy
s1 >>- s2 =
    do
       l1 <- s1
       l2 <- s2
       return $ l1 --((init l1)
                    -- ++ [replaceState (AntState $ length l1) (last l1)]) 
                    ++ l2 

sqC :: AntStrategy -> AntStrategy -> AntStrategy -> AntStrategy
sqC s1 s2 s3 =
    do
        l1 <- s1
        l2 <- s2
        l3 <- s3
        return $ (init l1
                    ++ [modifyState (incStateBy (length l2)) (last l1)])
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


