module AntMap where

import AntGen
import qualified Data.Map as M
import Control.Monad.Supply
import Data.Maybe (fromJust)


data AntStrategy' = AntStrategy'
    { instructions :: M.Map AntState AntInstruction
    , initial :: AntState
    , final :: AntState }
    deriving Show


type AntStrategy = Supply AntState AntStrategy'


aDrop :: AntStrategy
aDrop = do
    idx <- supply
    return $ AntStrategy' (M.fromList [(idx, Drop idx)]) idx idx


aTurn :: Dexterity -> AntStrategy
aTurn d = do
    idx <- supply
    return $ AntStrategy' (M.fromList [(idx, Turn d idx)]) idx idx

aSense :: Direction -> Condition -> AntStrategy -> AntStrategy
aSense d c exception = do
    idx <- supply
    e <- exception
    return $ AntStrategy' (M.insert idx (Sense d idx (initial e) c) (instructions e)) 
                          idx 
                          idx

{-
-- No infinite loops, for now
aLoop :: AntStrategy -> AntStrategy
aLoop as = do
    as' <- as
    let asFinalInstr = fromJust $ M.lookup (final as') (instructions as') 
        newInstrs    = M.insert (final as') 
                                (replaceDefaultState (initial as') asFinalInstr) 
                                (instructions as')
    return $ AntStrategy' newInstrs (initial as') (final as')
-}

data AntTest = TryForward
             | TryPickup
             | TrySense Direction Condition

-- TODO: finish all patterns
aWhile :: AntTest -> AntStrategy -> AntStrategy -> AntStrategy
aWhile TryForward s1 s2 = do
    s1' <- s1
    s2' <- s2
    idx <- supply
    let testInstr = Move t f
        (t,f)     = (initial s1',initial s2')
        s1''      = replaceFinal idx s1'
    return $ AntStrategy' (M.insert idx testInstr ((instructions s1'') `M.union` 
                                                   (instructions s2')))
                          idx
                          (final s2')
aWhile _ _ _ = undefined

replaceFinal :: AntState -> AntStrategy' -> AntStrategy'
replaceFinal idx as = 
    let asFinalInstr = fromJust $ M.lookup (final as) (instructions as) 
        newInstrs    = M.insert (final as) 
                                (replaceDefaultState idx asFinalInstr) 
                                (instructions as)
     in AntStrategy' newInstrs (initial as) (final as) 
    

runAnt p = fst $ runSupply p [AntState 0..]    

replaceDefaultState :: AntState -> AntInstruction -> AntInstruction
replaceDefaultState ns (Sense d _ s c) = (Sense d ns s c)
replaceDefaultState ns (Mark p _)      = (Mark p ns)
replaceDefaultState ns (UnMark p _)    = (UnMark p ns)
replaceDefaultState ns (PickUp _ s)    = (PickUp ns s)
replaceDefaultState ns (Drop _)        = (Drop ns)
replaceDefaultState ns (Turn d _)      = (Turn d ns)
replaceDefaultState ns (Move _ s)      = (Move ns s)
replaceDefaultState ns (Flip i s _)    = (Flip i s ns)


(>>-) :: AntStrategy -> AntStrategy -> AntStrategy
s1 >>- s2 = do
    s1' <- s1
    s2' <- s2
    let s1'FinalInstruction = fromJust $ M.lookup (final s1') (instructions s1')
        s1'NewInstructions  =
            M.insert (final s1')
                     (replaceDefaultState (initial s2') s1'FinalInstruction)
                     (instructions s1')
    return $ AntStrategy' (s1'NewInstructions `M.union` (instructions s2')) (initial s1') (final s2')
