module Game.UUAntGen.Backend.AntTransformation where

import           Control.Monad.Supply (runSupply)
import           Data.List            (delete)
import           Data.HashMap.Strict  ((!))
import qualified Data.HashMap.Strict  as M

import Game.UUAntGen.Backend.AntAssembly
import Game.UUAntGen.Backend.AntInstruction


-- | Obtains the next possible states of a given AntInstruction
getAntStates :: AntInstruction -> [AntState]
getAntStates (Sense _ s1 s2 _) = [s2,s1]
getAntStates (Mark _ s1)       = [s1]
getAntStates (UnMark _ s1)     = [s1]
getAntStates (PickUp s1 s2)    = [s2,s1]
getAntStates (Drop s1)         = [s1]
getAntStates (Turn _ s1)       = [s1]
getAntStates (Move s1 s2)      = [s2,s1]
getAntStates (Flip _   s1 s2)  = [s1,s2]
getAntStates (Ghost s1 ss)     = s1 : ss

getDefaultState :: AntInstruction -> AntState
getDefaultState = head . getAntStates


-- | Helper function, replaces the default (next) state of an instruction
replaceDefaultState :: AntState -> AntInstruction -> AntInstruction
replaceDefaultState ns (Sense d s _ c) = (Sense d s ns c)
replaceDefaultState ns (Flip i _ s)    = (Flip i ns s)
replaceDefaultState ns (Move s _)      = (Move s ns)
replaceDefaultState ns (PickUp s _)    = (PickUp s ns)
replaceDefaultState ns (Ghost _ ss)    = (Ghost ns ss) 
replaceDefaultState ns (Mark p _)      = (Mark p ns)
replaceDefaultState ns (UnMark p _)    = (UnMark p ns)
replaceDefaultState ns (Drop _)        = (Drop ns)
replaceDefaultState ns (Turn d _)      = (Turn d ns)


-- | Replaces the default (next) state in the final instruction of a strategy
replaceFinal :: AntState -> AntStrategy' -> AntStrategy'
replaceFinal idx (AntStrategy' m i f) = AntStrategy' newis i f where
    finali = m ! f
    newis  = M.insert f (replaceDefaultState idx finali) m


-- | Given a AntStrategy inside the Supply monad, runs the monad with a convenient
-- default supply of AntStates
runAntStrategy :: AntStrategy -> AntStrategy'
runAntStrategy s = fst $ runSupply s [AntState 0..]



-- PROGRAM TRANSFORMATION acting on the map of assembly instructions

-- | Makes a program loop on the top level (last instruction points to the first)
topLevelForever :: AntStrategy' -> AntStrategy'
topLevelForever as@(AntStrategy' _ i _) = replaceFinal i as


-- | Given a AntStrategy with possibly ghost AntInstructions (resulting from IfThenElse and IfThen
-- constructs), remove the Ghost instructions and make its parents bypass the ghost
-- POST-CONDITIONS:
--     * There are no (Ghost _ _) instructions in the instruction map
ghostBuster :: AntStrategy' -> AntStrategy'
ghostBuster (AntStrategy' m i f) = AntStrategy' (bypassGhosts m gs) i f -- replaces old instrs
    where
        gs = M.keys $ M.filter isGhost m
        isGhost (Ghost _ _ ) = True
        isGhost _            = False

bypassGhosts :: IMap -> [AntState] -> IMap
bypassGhosts m gs = foldl bypassGhost m gs

bypassGhost :: IMap -> AntState -> IMap
bypassGhost m gidx = foldr (M.adjust bypassP) (M.delete gidx m) ss
    where
        (Ghost n ss) = m ! gidx
        bypassP      = replaceMatchingStates gidx n

-- TODO UGLY NAME Replace a state by a new if it matches the wanted one
r :: AntState -> AntState -> AntState -> AntState
r wanted existing new = if wanted == existing then new else existing

replaceMatchingStates :: AntState -> AntState -> AntInstruction -> AntInstruction
replaceMatchingStates o n (Sense d s0 s1 c) = (Sense  d (r o s0 n) (r o s1 n) c          )
replaceMatchingStates o n (Flip i s0 s1)    = (Flip   i (r o s0 n) (r o s1 n)            )
replaceMatchingStates o n (Move s0 s1)      = (Move     (r o s0 n) (r o s1 n)            )
replaceMatchingStates o n (PickUp s0 s1)    = (PickUp   (r o s0 n) (r o s1 n)            )
replaceMatchingStates o n (Ghost s0 ss)     = (Ghost    (r o s0 n) (map (\s -> r o s n) ss))
replaceMatchingStates o n (Mark p s0)       = (Mark   p (r o s0 n)                       )
replaceMatchingStates o n (UnMark p s0)     = (UnMark p (r o s0 n)                       )
replaceMatchingStates o n (Drop s0)         = (Drop     (r o s0 n)                       )
replaceMatchingStates o n (Turn d s0)       = (Turn   d (r o s0 n)                       )


-- | The keyspace of an instruction map might be arbitrary. This function translates the key space
-- to range from 0 to (size - 1). This resulting space can be considered as "line numbers".
-- POST-CONDITIONS:
--     * The keyspace of the instruction map is in the range [0, (size-1)]
--     * The key of the initial instruction is 0
keysToLineNumbers :: AntStrategy' -> IMap
keysToLineNumbers (AntStrategy' m i _) = keySpaceTranslate m from0
    where from0 = (i, AntState 0) : zip (delete i $ M.keys m) [(AntState 1)..]

keySpaceTranslate :: IMap -> [(AntState, AntState)] -> IMap
keySpaceTranslate orig translations = foldl keyTranslate orig translations

keyTranslate :: IMap -> (AntState, AntState) -> IMap
keyTranslate m (k, nk) = M.map (replaceMatchingStates k nk) changedInst
    where
        ins = m ! k
        changedInst = M.insert nk ins $ M.delete k m



-- | Pretty-printing an instruction map to a String.
-- PRE-CONDITIONS:
--     * The instruction map has keys in the range [0, (size-1)]
--     * The key of the initial instruction is 0
printInstructions :: IMap -> String
printInstructions m = unlines $ map show (M.elems m)

-- Prints an instruction map, but shows also the keys (for better debugging)
printInstructionsWithKeys :: IMap -> String
printInstructionsWithKeys m = unlines $ map show (M.toList m)


