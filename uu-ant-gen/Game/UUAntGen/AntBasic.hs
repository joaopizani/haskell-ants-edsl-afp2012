module Game.UUAntGen.AntBasic where

import Game.UUAntGen.AntInstruction

import qualified Data.Map as M
import Control.Monad.Supply

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

aMoveOrWall :: AntStrategy -> AntStrategy
aMoveOrWall ws = do
    ws' <- ws
    idx <- supply
    gidx <- supply
    let ghost = (gidx, Ghost gidx idx idx)
        allInstrs = M.fromList [ghost, (idx, Move gidx (initial ws'))] `M.union` (instructions ws')
    return $ AntStrategy' allInstrs idx gidx 

aPickup :: AntStrategy -> AntStrategy
aPickup ws = do
    idx <- supply
    ws' <- ws
    gidx <- supply
    let ghost = (gidx, Ghost gidx idx idx)
        allInstrs = M.fromList [ghost, (idx, PickUp gidx (initial ws'))] `M.union` (instructions ws')
    return $ AntStrategy' allInstrs idx gidx 


