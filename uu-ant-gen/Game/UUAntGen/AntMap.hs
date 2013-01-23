module Game.UUAntGen.AntMap where

import qualified Data.Map as M
import Data.Map ((!))
import Data.Maybe (fromJust)
import Data.List (delete)
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
getDefaultState :: AntInstruction -> AntState
getDefaultState (Sense _ _ s _) = s
getDefaultState (Mark _ s)      = s
getDefaultState (UnMark _ s)    = s
getDefaultState (PickUp _ s)    = s
getDefaultState (Drop s)        = s
getDefaultState (Turn _ s)      = s
getDefaultState (Move _ s)      = s
getDefaultState (Flip _ s _)    = s


-- | Helper function, replaces the default (next) state of an instruction
replaceDefaultState :: AntState -> AntInstruction -> AntInstruction
replaceDefaultState ns (Sense d s _ c) = (Sense d s ns c)
replaceDefaultState ns (Flip i _ s)    = (Flip i ns s)
replaceDefaultState ns (Move s _)      = (Move s ns)
replaceDefaultState ns (PickUp s _)    = (PickUp s ns)
replaceDefaultState ns (Ghost _ a b)   = (Ghost ns a b)
replaceDefaultState ns (Mark p _)      = (Mark p ns)
replaceDefaultState ns (UnMark p _)    = (UnMark p ns)
replaceDefaultState ns (Drop _)        = (Drop ns)
replaceDefaultState ns (Turn d _)      = (Turn d ns)

-- | Replaces the default (next) state in the final instruction of a strategy
replaceFinal :: AntState -> AntStrategy' -> AntStrategy'
replaceFinal idx s = AntStrategy' newis (initial s) (final s) where
    finali = instructions s ! final s
    newis  = M.insert (final s) (replaceDefaultState idx finali) (instructions s)


-- | Sequencing two AntStrategies. Means that s2 will be executed after s1
(>>-) :: AntStrategy -> AntStrategy -> AntStrategy
s1 >>- s2 = do
    s1' <- s1  -- extracting instruction blocks from within the Supply monad
    s2' <- s2
    let s1final = instructions s1' ! final s1'
        s1new   = M.insert (final s1') (replaceDefaultState (initial s2') s1final) (instructions s1')
    return $ AntStrategy' (s1new `M.union` (instructions s2')) (initial s1') (final s2')


-- | Datatype representing all the possible tests to be performed in a conditional AntStrategy
data AntTest
    = TryForward
    | TryPickup
    | TrySense Direction Condition
    | TryRandomEqZero Int
    deriving Show


-- | While block, is given a test and a strategy for the body
aWhile :: AntTest -> AntStrategy -> AntStrategy
aWhile TryForward          = aMkWhile Move
aWhile TryPickup           = aMkWhile PickUp
aWhile (TrySense d c)      = aMkWhile $ \t f -> Sense d t f c
aWhile (TryRandomEqZero p) = aMkWhile $ flip (Flip p)

-- Helper function to aWhile. Produces a conditional loop block, given a conditional
-- assembly instruction and a strategies for the loop body
aMkWhile :: (AntState -> AntState -> AntInstruction) -> AntStrategy -> AntStrategy
aMkWhile condi b = do
    idx   <- supply  -- getting the unique id for the conditional instruction
    gidx  <- supply  -- getting the unique id for the ghost instruction
    b'    <- b  -- extracting the instruction blocks from the Supply monad
    let bidx         = initial b'
        testi        = condi bidx gidx
        ghosti       = Ghost gidx idx idx
        bPlusG       = M.insert gidx ghosti (instructions $ replaceFinal idx b')
    return $ AntStrategy' (M.insert idx testi bPlusG) idx gidx


-- | IfThenElse, given a test and two strategies: one for the true branch and one for the false
aIfThenElse :: AntTest -> AntStrategy -> AntStrategy -> AntStrategy
aIfThenElse TryForward          = aMkIfThenElse Move
aIfThenElse TryPickup           = aMkIfThenElse PickUp
aIfThenElse (TrySense d c)      = aMkIfThenElse $ \t f -> Sense d t f c
aIfThenElse (TryRandomEqZero p) = aMkIfThenElse $ flip (Flip p)

-- Helper function to aIfThenElse. Produces a conditional strategy given an assembly instruction
-- and two strategies. Introduces a "Ghost" instruction to serve as return point from both branches
aMkIfThenElse :: (AntState -> AntState -> AntInstruction) -> AntStrategy -> AntStrategy -> AntStrategy
aMkIfThenElse condi ts fs = do
    idx <- supply  -- getting the unique id for the testing instruction
    gidx <- supply -- getting the unique id for the ghost instruction
    ts' <- ts  -- extracting the instruction blocks from the Supply monad
    fs' <- fs
    let (tidx, fidx) = (initial ts', initial fs')
        ghosti       = Ghost gidx (final ts') (final fs')
        testi        = condi tidx fidx
        fPlusT       = (instructions $ replaceFinal gidx ts') `M.union` (instructions $ replaceFinal gidx fs')
    return $ AntStrategy' (M.insert idx testi (M.insert gidx ghosti fPlusT)) idx gidx



-- | Given a AntStrategy inside the Supply monad, runs the monad with a convenient
-- default supply of AntStates
getAntStrategy :: AntStrategy -> AntStrategy'
getAntStrategy s = fst $ runSupply s [AntState 0..]




-- | Given a AntStrategy with possibly ghost AntInstructions (resulting from IfThenElse and IfThen
-- constructs), remove the Ghost instructions and make its parents bypass the ghost
-- POST-CONDITIONS:
--     * There are no (Ghost _ _ _) instructions in the instruction map
ghostBuster :: AntStrategy' -> AntStrategy'
ghostBuster (AntStrategy' m i f) = AntStrategy' (bypassGhosts m gs) i f -- replaces old instrs
    where
        gs = M.keys $ M.filter isGhost m
        isGhost (Ghost _ _ _) = True
        isGhost _             = False

bypassGhosts :: IMap -> [AntState] -> IMap
bypassGhosts m gs = foldl bypassGhost m gs


-- TODO not bypassing correctly the ghosts in aWhile
bypassGhost :: IMap -> AntState -> IMap
bypassGhost m ghost = M.adjust bypassP p2 $ M.adjust bypassP p1 $ M.delete ghost m
    where
        (Ghost n p1 p2) = m ! ghost
        bypassP         = replaceDefaultState n



-- | The keyspace of an instruction map might be arbitrary. This function translates the key space
-- to range from 0 to (size - 1). This resulting space can be considered as "line numbers".
-- POST-CONDITIONS:
--     * The keyspace of the instruction map is in the range [0, (size-1)]
--     * The key of the initial instruction is 0
fromKeysToLineNumbers :: AntStrategy' -> AntStrategy'
fromKeysToLineNumbers (AntStrategy' m i f) = AntStrategy' (translateKeySpaceIMap m from0) newi newf
    where
        from0        = (i, AntState 0) : zip (delete i $ M.keys m) [(AntState 1)..]
        (newi, newf) = (fromJust $ lookup i from0, fromJust $ lookup f from0)

translateKeySpaceIMap :: IMap -> [(AntState, AntState)] -> IMap
translateKeySpaceIMap orig translations = foldl translateOneKey orig translations

translateOneKey :: IMap -> (AntState, AntState) -> IMap
translateOneKey m (k, nk) = M.map (replaceMatchingStates k nk) changedInst
    where
        ins = m ! k
        changedInst = M.insert nk ins $ M.delete k m

-- TODO UGLY NAME Replace a state by a new if it matches the wanted one
h :: AntState -> AntState -> AntState -> AntState
h wanted existing new = if wanted == existing then new else existing

replaceMatchingStates :: AntState -> AntState -> AntInstruction -> AntInstruction
replaceMatchingStates o n (Sense d s0 s1 c) = (Sense  d (h o s0 n) (h o s1 n) c          )
replaceMatchingStates o n (Flip i s0 s1)    = (Flip   i (h o s0 n) (h o s1 n)            )
replaceMatchingStates o n (Move s0 s1)      = (Move     (h o s0 n) (h o s1 n)            )
replaceMatchingStates o n (PickUp s0 s1)    = (PickUp   (h o s0 n) (h o s1 n)            )
replaceMatchingStates o n (Ghost s0 s1 s2)  = (Ghost    (h o s0 n) (h o s1 n) (h o s2 n) )
replaceMatchingStates o n (Mark p s0)       = (Mark   p (h o s0 n)                       )
replaceMatchingStates o n (UnMark p s0)     = (UnMark p (h o s0 n)                       )
replaceMatchingStates o n (Drop s0)         = (Drop     (h o s0 n)                       )
replaceMatchingStates o n (Turn d s0)       = (Turn   d (h o s0 n)                       )



-- | Pretty-printing an AntStrategy' to a String.
-- PRE-CONDITIONS:
--     * The instruction map has keys in the range [0, (size-1)]
--     * The key of the initial instruction is 0
printAntStrategy' :: AntStrategy' -> String
printAntStrategy' (AntStrategy' m _ _) = unlines $ map show (M.elems m)

