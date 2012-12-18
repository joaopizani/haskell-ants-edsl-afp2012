module AntStrategies where

import AntGen

import Control.Monad.State

type AntStrategy = State StateID [AntInstruction] 

type StateID = Int

drop :: AntStrategy 
drop = do 
    s <- get
    modify (+1)
    return $ singleton $ Drop (AntState s)

turn :: Dexterity -> AntStrategy
turn d = do
    s <- get
    modify (+1)
    return $ singleton $ Turn d (AntState s)

singleton = (:[])

sequence :: AntStrategy -> AntStrategy -> AntStrategy
sequence s1 s2 =
    do
       l1 <- s1
       l2 <- s2
       return $ (init l1 ++ [replaceState (AntState 42) --TODO instruction must have an ID
                                          (last l1)]) ++ l2

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


