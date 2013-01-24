module Game.UUAntGen.AntTransformation where

import Game.UUAntGen.AntInstruction
import Game.UUAntGen.AntBasic

import qualified Data.Map as M 
import Data.Map             ((!)) 
import Data.Maybe           (fromJust)
import Data.List            (delete)
import Control.Monad.Supply (runSupply)

getAntStates :: AntInstruction -> [AntState] 
getAntStates (Sense _ s1 s2 _) = [s2,s1]
getAntStates (Mark _ s1)       = [s1]
getAntStates (UnMark _ s1)     = [s1]
getAntStates (PickUp s1 s2)    = [s2,s1]
getAntStates (Drop s1)         = [s1]
getAntStates (Turn _ s1)       = [s1]
getAntStates (Move s1 s2)      = [s2,s1]
getAntStates (Flip _   s1 s2)  = [s1,s2]
getAntStates (Ghost s1 s2 s3)  = [s1,s2,s3]

getDefaultState :: AntInstruction -> AntState
getDefaultState = head . getAntStates

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
bypassGhost m gidx = M.adjust bypassP p2 $ M.adjust bypassP p1 $ M.delete gidx m
    where
        (Ghost n p1 p2) = m ! gidx
        bypassP         = replaceMatchingStates gidx n

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




-- | Pretty-printing an AntStrategy' to a String.
-- PRE-CONDITIONS:
--     * The instruction map has keys in the range [0, (size-1)]
--     * The key of the initial instruction is 0
printAntStrategy' :: AntStrategy' -> String
printAntStrategy' (AntStrategy' m _ _) = unlines $ map show (M.elems m)

