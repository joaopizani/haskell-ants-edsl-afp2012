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
