module Game.UUAntGen.AntMap where

import qualified Data.Map as M
import Control.Monad.Supply
import Game.UUAntGen.AntInstruction


-- Just for the case when we want to change the map implementation to something else
type IMap = M.Map AntState AntInstruction

data AntStrategy' = AntStrategy'
    { instructions :: IMap
    , initial :: AntState
    , final :: AntState }
    deriving Eq

instance Show AntStrategy' where
    show (AntStrategy' i s0 sf) = unlines ["initial = " ++ show s0, "final = " ++ show sf, show i]

type AntStrategy = Supply AntState AntStrategy'


aMkSingletonStrategy' :: (AntState -> AntInstruction) -> AntState -> AntStrategy'
aMkSingletonStrategy' instr idx = AntStrategy' (M.fromList [(idx, instr idx)]) idx idx

-- Basic instructions, one function per assembly instruction
aMkSingletonStrategy :: (AntState -> AntInstruction) -> AntStrategy
aMkSingletonStrategy instr = do
    idx <- supply
    return $ aMkSingletonStrategy' instr idx


aMark :: Pheromone -> AntStrategy
aMark p = aMkSingletonStrategy (Mark p)

aUnMark :: Pheromone -> AntStrategy
aUnMark p = aMkSingletonStrategy (UnMark p)

aDrop :: AntStrategy
aDrop = aMkSingletonStrategy Drop

aTurn :: Dexterity -> AntStrategy
aTurn d = aMkSingletonStrategy (Turn d)

aTurnL :: AntStrategy
aTurnL = aTurn L

aTurnR :: AntStrategy
aTurnR = aTurn R



-- Composing AntStrategies. Sequencing, loop, conditionals, etc.

-- | Helper function, replaces the default (next) state of an instruction
replaceDefaultState :: AntState -> AntInstruction -> AntInstruction
replaceDefaultState ns (Sense d _ s c) = (Sense d ns s c)
replaceDefaultState ns (Mark p _)      = (Mark p ns)
replaceDefaultState ns (UnMark p _)    = (UnMark p ns)
replaceDefaultState ns (PickUp _ s)    = (PickUp ns s)
replaceDefaultState ns (Drop _)        = (Drop ns)
replaceDefaultState ns (Turn d _)      = (Turn d ns)
replaceDefaultState ns (Move _ s)      = (Move ns s)
replaceDefaultState ns (Flip i s _)    = (Flip i s ns)
replaceDefaultState ns (Ghost _ a b)   = (Ghost ns a b)

-- | Replaces the default (next) state in the final instruction of a strategy
replaceFinal :: AntState -> AntStrategy' -> AntStrategy'
replaceFinal idx s = AntStrategy' newis (initial s) (final s) where
    finali = instructions s  M.! final s
    newis  = M.insert (final s) (replaceDefaultState idx finali) (instructions s)


-- | Sequencing two AntStrategies. Means that s2 will be executed after s1
(>>-) :: AntStrategy -> AntStrategy -> AntStrategy
s1 >>- s2 = do
    s1' <- s1  -- extracting instruction blocks from within the Supply monad
    s2' <- s2
    let s1final = instructions s1'  M.!  final s1'
        s1new   = M.insert (final s1') (replaceDefaultState (initial s2') s1final) (instructions s1')
    return $ AntStrategy' (s1new `M.union` (instructions s2')) (initial s1') (final s2')


-- | Datatype representing all the possible tests to be performed in a conditional AntStrategy
data AntTest
    = TryForward
    | TryPickup
    | TrySense Direction Condition
    | TryRandomEqZero Int


-- Helper function to make conditional strategies. Performs "translation" between a user-accessible
-- boolean test (AntTest) and a ant assembly instruction to perform that test.
aCond :: ((AntState -> AntState -> AntInstruction) -> AntStrategy -> AntStrategy -> AntStrategy)
        -> AntTest -> AntStrategy -> AntStrategy -> AntStrategy
aCond f TryForward           = f Move
aCond f TryPickup            = f PickUp
aCond f (TrySense d c)       = f $ \t f' -> Sense d t f' c
aCond f (TryRandomEqZero p)  = f $ flip (Flip p)


-- | While block, is given a test, a strategy for while true and a strategy for outside the loop
aWhile :: AntTest -> AntStrategy -> AntStrategy -> AntStrategy
aWhile = aCond aMkWhile

-- Helper function to aWhile. Produces a conditional loop block, given a conditional assembly
-- instruction and two strategies, one for the true and one for the false branch
aMkWhile :: (AntState -> AntState -> AntInstruction) -> AntStrategy -> AntStrategy -> AntStrategy
aMkWhile condi ts fs = do
    idx <- supply  -- getting the unique id for the conditional instruction
    ts' <- ts  -- extracting the instruction blocks from the Supply monad
    fs' <- fs
    let (tidx, fidx) = (initial ts', initial fs')
        testi        = condi tidx fidx
        fPlusT       = (instructions $ replaceFinal idx ts') `M.union` (instructions fs')
    return $ AntStrategy' (M.insert idx testi fPlusT) idx (final fs')


-- | IfThenElse, given a test and two strategies: one for the true branch and one for the false
aIfThenElse :: AntTest -> AntStrategy -> AntStrategy -> AntStrategy
aIfThenElse = aCond aMkIfThenElse

-- Helper function to aIfThenElse. Produces a conditional strategy given an assembly instruction
-- and two strategies. Introduces a "Ghost" instruction to serve as return point from both branches
aMkIfThenElse :: (AntState -> AntState -> AntInstruction) -> AntStrategy -> AntStrategy -> AntStrategy
aMkIfThenElse condi ts fs = do
    idx <- supply  -- getting the unique id for the mutual end instruction
    idx' <- supply -- getting the unique id for conditional instruction
    ts' <- ts  -- extracting the instruction blocks from the Supply monad
    fs' <- fs
    let (tidx, fidx) = (initial ts', initial fs')
        ghosti       = Ghost idx' (final ts') (final fs')
        testi        = condi tidx fidx
        fPlusT       = (instructions $ replaceFinal idx' ts') `M.union` (instructions $ replaceFinal idx' fs')
    return $ AntStrategy' (M.insert idx testi (M.insert idx' ghosti fPlusT)) idx idx' 


-- | Given a AntStrategy inside the Supply monad, runs the monad with a convenient
-- default supply of AntStates
getAntStrategy :: AntStrategy -> AntStrategy'
getAntStrategy s = fst $ runSupply s [AntState 0..]


ghostBuster :: AntStrategy -> AntStrategy
ghostBuster is = do
    is' <- is
    let isGhost (Ghost _ _ _) = True
        isGhost _             = False
        imap = instructions is'
        gs   = M.keys (M.filter isGhost imap)
    return $ AntStrategy' (bypassGhosts imap gs) (initial is') (final is') -- replaces old instrs


bypassGhosts :: IMap -> [AntState] -> IMap
bypassGhosts m gs = foldl bypassGhost m gs

bypassGhost :: IMap -> AntState -> IMap
bypassGhost m ghost = M.adjust bypassP p2 $ M.adjust bypassP p1 $ M.delete ghost m
    where
        (Ghost n p1 p2) = m M.! ghost
        bypassP         = replaceDefaultState n


printStrategy :: AntStrategy -> String
printStrategy s = unlines $ map printAntLine [(AntState 0)..(AntState (smax - 1))] 
    where
        (AntStrategy' imap _ _)  = getAntStrategy s
        printAntLine i           = maybe "Drop 4" show $ M.lookup i imap
        ((AntState smax), _)     = M.findMax imap

