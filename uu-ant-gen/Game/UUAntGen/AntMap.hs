module Game.UUAntGen.AntMap where

import qualified Data.Map as M
import Data.Maybe (fromJust)
import Control.Monad.Supply
import Game.UUAntGen.AntInstruction


data AntStrategy' = AntStrategy'
    { instructions :: M.Map AntState AntInstruction
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

{- TODO remove this, only way of creating conditional instructions is by using while or if
aSense :: Direction -> Condition -> AntStrategy -> AntStrategy
aSense d c exception = do
    idx <- supply
    e <- exception
    return $ AntStrategy' (M.insert idx (Sense d idx (initial e) c) (instructions e)) 
                          idx
                          idx
-}



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
    finali = fromJust $ M.lookup (final s) (instructions s)
    newis  = M.insert (final s) (replaceDefaultState idx finali) (instructions s)


-- | Sequencing two AntStrategies. Means that s2 will be executed after s1
(>>-) :: AntStrategy -> AntStrategy -> AntStrategy
s1 >>- s2 = do
    s1' <- s1  -- extracting instruction blocks from within the Supply monad
    s2' <- s2
    let s1final = fromJust $ M.lookup (final s1') (instructions s1')
        s1new   = M.insert (final s1') (replaceDefaultState (initial s2') s1final) (instructions s1')
    return $ AntStrategy' (s1new `M.union` (instructions s2')) (initial s1') (final s2')


-- | Datatype representing all the possible tests to be performed in a conditional AntStrategy
data AntTest
    = TryForward
    | TryPickup
    | TrySense Direction Condition
    | TryRandomEqZero Int


-- | While block, is given a test, a strategy for while true and a strategy for outside the loop
aCond :: ((AntState -> AntState -> AntInstruction) -> AntStrategy -> AntStrategy -> AntStrategy)
        -> AntTest -> AntStrategy -> AntStrategy -> AntStrategy
aCond f TryForward           = f Move
aCond f TryPickup            = f PickUp
aCond f (TrySense d c)       = f $ \t f' -> Sense d t f' c
aCond f (TryRandomEqZero p)  = f $ flip (Flip p)

aWhile = aCond aMkWhile

-- Helper function to aWhile
aMkWhile :: (AntState -> AntState -> AntInstruction) -> AntStrategy -> AntStrategy -> AntStrategy
aMkWhile condi ts fs = do
    idx <- supply  -- getting the unique id for the conditional instruction
    ts' <- ts  -- extracting the instruction blocks from the Supply monad
    fs' <- fs
    let (tidx, fidx) = (initial ts', initial fs')
        testi        = condi tidx fidx
        fPlusT       = (instructions $ replaceFinal idx ts') `M.union` (instructions fs')
    return $ AntStrategy' (M.insert idx testi fPlusT) idx (final fs')


-- if-then-else in a similar style to aWhile, then if-without-else using if-then-else
aIfThenElse :: AntTest -> AntStrategy -> AntStrategy -> AntStrategy
aIfThenElse = aCond aMkIfThenElse

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
    let imap = instructions $ is'
        (ghosts, noGhosts) = M.partition isGhost imap
        newPredsMap = M.unions $ M.elems $ M.map (M.fromList . (redirectGhostPred imap)) ghosts
    return $ AntStrategy' (M.union newPredsMap noGhosts) (initial is') (final is')  -- left-biased, prefers new instructions
    where isGhost (Ghost _ _ _ ) = True
          isGhost _              = False

redirectGhostPred :: M.Map AntState AntInstruction -> AntInstruction
                        -> [(AntState, AntInstruction)]  --TODO THIS IS UGLY
redirectGhostPred map (Ghost n p1 p2) = let pi1 = fromJust $ M.lookup p1 map  -- UHHHHH
                                            pi2 = fromJust $ M.lookup p2 map  -- UUHHHH fromJust
                                            pi1' = replaceDefaultState n pi1
                                            pi2' = replaceDefaultState n pi2
                                            in [(p1, pi1'), (p2, pi2')]
redirectGhostPred _ _ = undefined 

printStrategy :: AntStrategy -> String
printStrategy s = unlines $ map printAntLine [(AntState 0)..(AntState (max-1))] 
    where
        (AntStrategy' imap init final) = getAntStrategy s
        printAntLine i                 = maybe "Drop 4" show $ M.lookup i imap
        ((AntState max), _)            = M.findMax imap
        
