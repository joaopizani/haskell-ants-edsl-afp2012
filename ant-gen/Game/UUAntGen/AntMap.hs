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


-- Basic instructions, one function per assembly instruction
aMkSingletonStrategy :: (AntState -> AntInstruction) -> AntStrategy
aMkSingletonStrategy instr = do
    idx <- supply
    return $ AntStrategy' (M.fromList [(idx, instr idx)]) idx idx


aMark :: Pheromone -> AntStrategy
aMark p = aMkSingletonStrategy (Mark p)

aUnMark :: Pheromone -> AntStrategy
aUnMark p = aMkSingletonStrategy (UnMark p)

aDrop :: AntStrategy
aDrop = aMkSingletonStrategy Drop

aTurn :: Dexterity -> AntStrategy
aTurn d = aMkSingletonStrategy (Turn d)



{- TODO remove this, only way of creating conditional instructions is by using while or if
aSense :: Direction -> Condition -> AntStrategy -> AntStrategy
aSense d c exception = do
    idx <- supply
    e <- exception
    return $ AntStrategy' (M.insert idx (Sense d idx (initial e) c) (instructions e)) 
                          idx
                          idx
-}

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


replaceFinal :: AntState -> AntStrategy' -> AntStrategy'
replaceFinal idx as = 
    let asFinalInstr = fromJust $ M.lookup (final as) (instructions as) 
        newInstrs    = M.insert (final as) 
                                (replaceDefaultState idx asFinalInstr) 
                                (instructions as)
     in AntStrategy' newInstrs (initial as) (final as) 


data AntTest
    = TryForward
    | TryPickup
    | TrySense Direction Condition
    | TryRandomEqZero Int


aWhile :: AntTest -> AntStrategy -> AntStrategy -> AntStrategy
aWhile TryForward           = aMkWhileBlk Move
aWhile TryPickup            = aMkWhileBlk PickUp
aWhile (TrySense d c)       = aMkWhileBlk $ \t f -> Sense d t f c
aWhile (TryRandomEqZero p)  = aMkWhileBlk $ flip (Flip p)


aMkWhileBlk :: (AntState -> AntState -> AntInstruction) -> AntStrategy -> AntStrategy -> AntStrategy
aMkWhileBlk conditional trueBlk falseBlk = do
    trueBlk'  <- trueBlk
    falseBlk' <- falseBlk
    idx       <- supply
    let
        testInstr     = conditional true false
        (true, false) = (initial trueBlk',initial falseBlk')
        trueBlk''     = replaceFinal idx trueBlk'
    return $ AntStrategy'
        (M.insert idx testInstr ((instructions trueBlk'') `M.union` (instructions falseBlk')))
        idx
        (final falseBlk')



-- TODO write if-then-else in a similar style to aWhile, then if-without-else using if-then-else
runAnt p = fst $ runSupply p [AntState 0..]

