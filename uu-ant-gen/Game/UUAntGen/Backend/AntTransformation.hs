module Game.UUAntGen.Backend.AntTransformation where

import           Control.Monad.Supply (runSupply)
import           Control.Monad.Reader (runReaderT)
import           Data.List            (delete)
import           Data.Maybe           (fromJust)
import           Data.Map             ((!))
import qualified Data.Map             as M

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

getDefaultState :: AntInstruction -> AntState
getDefaultState = head . getAntStates


-- | Helper function, replaces the default (next) state of an instruction
replaceDefaultState :: AntState -> AntInstruction -> AntInstruction
replaceDefaultState ns (Sense d s _ c) = (Sense d s ns c)
replaceDefaultState ns (Flip i _ s)    = (Flip i ns s)
replaceDefaultState ns (Move s _)      = (Move s ns)
replaceDefaultState ns (PickUp s _)    = (PickUp s ns)
replaceDefaultState ns (Mark p _)      = (Mark p ns)
replaceDefaultState ns (UnMark p _)    = (UnMark p ns)
replaceDefaultState ns (Drop _)        = (Drop ns)
replaceDefaultState ns (Turn d _)      = (Turn d ns)


-- | Running an AntStrategy and retrieving the initial state, set of final states and the
-- IMap.
runAntStrategy :: AntStrategy -> AntStrategy' 
runAntStrategy as = let supply      = runReaderT as i -- topLevelForever by using initial 
                        ((i,f,m),_) = runSupply supply [(AntState 0)..]
                     in (i,f,m)


-- PROGRAM TRANSFORMATION acting on the map of assembly instructions


r :: AntState -> AntState -> AntState -> AntState
r wanted existing new = if wanted == existing then new else existing

replaceMatchingStates :: AntState -> AntState -> AntInstruction -> AntInstruction
replaceMatchingStates o n (Sense d s0 s1 c) = (Sense  d (r o s0 n) (r o s1 n) c          )
replaceMatchingStates o n (Flip i s0 s1)    = (Flip   i (r o s0 n) (r o s1 n)            )
replaceMatchingStates o n (Move s0 s1)      = (Move     (r o s0 n) (r o s1 n)            )
replaceMatchingStates o n (PickUp s0 s1)    = (PickUp   (r o s0 n) (r o s1 n)            )
replaceMatchingStates o n (Mark p s0)       = (Mark   p (r o s0 n)                       )
replaceMatchingStates o n (UnMark p s0)     = (UnMark p (r o s0 n)                       )
replaceMatchingStates o n (Drop s0)         = (Drop     (r o s0 n)                       )
replaceMatchingStates o n (Turn d s0)       = (Turn   d (r o s0 n)                       )


-- | Switches the initial instruction with instruction nr 0. 
-- POST-CONDITIONS:
--     * The key of the initial instruction is 0
setInitialAsZero :: AntStrategy' -> IMap
setInitialAsZero (i,_,m') = let m       = M.map exchangeStates m'
                                zero    = fromJust $ M.lookup aZero m 
                                initial = fromJust $ M.lookup i m
                                newMap  = M.insert i zero 
                                        $ M.insert aZero initial
                                        $ M.delete i
                                        $ M.delete aZero m
                              in newMap  
    where 
        aZero = AntState 0
        -- sorry about this boilerplate :(
        exchangeStates (Mark p s) | s == i     = Mark p aZero 
                                  | s == aZero = Mark p i
                                  | otherwise  = Mark p s
        exchangeStates (UnMark p s) | s == i     = UnMark p aZero 
                                    | s == aZero = UnMark p i
                                    | otherwise  = UnMark p s
        exchangeStates (Drop s) | s == i     = Drop aZero 
                                | s == aZero = Drop i
                                | otherwise  = Drop s
        exchangeStates (Turn d s) | s == i     = Turn d aZero 
                                  | s == aZero = Turn d i
                                  | otherwise  = Turn d s
        exchangeStates (Sense d s1 s2 c) | s1 == i && s2 == aZero     = Sense d aZero i c
                                         | s1 == aZero && s2 == i     = Sense d i aZero c
                                         | s1 == aZero && s2 == aZero = Sense d i i c
                                         | s1 == i && s2 == i         = Sense d aZero 
                                                                              aZero c
                                         | s1 == aZero                = Sense d i s2 c
                                         | s1 == i                    = Sense d aZero s2 c
                                         | s2 == aZero                = Sense d s1 i c
                                         | s2 == i                    = Sense d s1 aZero c
                                         | otherwise                  = Sense d s1 s2 c
        exchangeStates (Move s1 s2) | s1 == i && s2 == aZero     = Move aZero i
                                    | s1 == aZero && s2 == i     = Move i aZero
                                    | s1 == aZero && s2 == aZero = Move i i
                                    | s1 == i && s2 == i         = Move aZero aZero
                                    | s1 == aZero                = Move i s2
                                    | s1 == i                    = Move aZero s2
                                    | s2 == aZero                = Move s1 i
                                    | s2 == i                    = Move s1 aZero
                                    | otherwise                  = Move s1 s2
        exchangeStates (PickUp s1 s2) | s1 == i && s2 == aZero     = PickUp aZero i
                                      | s1 == aZero && s2 == i     = PickUp i aZero
                                      | s1 == aZero && s2 == aZero = PickUp i i
                                      | s1 == i && s2 == i         = PickUp aZero aZero
                                      | s1 == aZero                = PickUp i s2
                                      | s1 == i                    = PickUp aZero s2
                                      | s2 == aZero                = PickUp s1 i
                                      | s2 == i                    = PickUp s1 aZero
                                      | otherwise                  = PickUp s1 s2
        exchangeStates (Flip p s1 s2) | s1 == i && s2 == aZero     = Flip p aZero i
                                      | s1 == aZero && s2 == i     = Flip p i aZero
                                      | s1 == aZero && s2 == aZero = Flip p i i
                                      | s1 == i && s2 == i         = Flip p aZero aZero
                                      | s1 == aZero                = Flip p i s2
                                      | s1 == i                    = Flip p aZero s2
                                      | s2 == aZero                = Flip p s1 i
                                      | s2 == i                    = Flip p s1 aZero
                                      | otherwise                  = Flip p s1 s2
  
  
-- | Pretty-printing an instruction map to a String.
-- PRE-CONDITIONS:
--     * The instruction map has keys in the range [0, (size-1)]
--     * The key of the initial instruction is 0
printInstructions :: IMap -> String
printInstructions m = unlines $ map show (M.elems m)

-- Prints an instruction map, but shows also the keys (for better debugging)
printInstructionsWithKeys :: IMap -> String
printInstructionsWithKeys m = unlines $ map show (M.toList m)


