module Game.UUAntGen.Backend.AntInstruction where

import           Control.Monad.Supply (Supply, supply)
import qualified Data.HashMap.Strict  as M

import Game.UUAntGen.Backend.AntAssembly
import Game.UUAntGen.Backend.AntDeepEmbedded


type IMap = M.HashMap AntState AntInstruction

data AntStrategy' = AntStrategy'
    { instructions :: IMap
    , initial      :: AntState
    , final        :: AntState }
    deriving Eq

instance Show AntStrategy' where
    show (AntStrategy' i s0 sf) = unlines ["initial = " ++ show s0, "final = " ++ show sf, show i]


-- | The AntStrategy' type, wrapped in the Supply monad for obtaining unique instruction ids
type AntStrategy = Supply AntState AntStrategy'


-- Basic strategies, one function per assembly word. Each basic strategy has a "real assembly"
-- version (of type AntStrategy), and a "deep embedded" EDSL version (of type AntImperative).
-- The conversion AntBasic -> AntStrategy is done by the semanticsBasic function
aMkSingletonStrategy' :: (AntState -> AntInstruction) -> AntState -> AntStrategy'
aMkSingletonStrategy' instr idx = AntStrategy' (M.fromList [(idx, instr idx)]) idx idx

aMkSingletonStrategy :: (AntState -> AntInstruction) -> AntStrategy
aMkSingletonStrategy instr = supply >>= return . aMkSingletonStrategy' instr


aMark :: Pheromone -> AntStrategy
aMark p = aMkSingletonStrategy (Mark p)

iMark :: Pheromone -> AntImperative
iMark p = Single $ CMark p


aUnMark :: Pheromone -> AntStrategy
aUnMark p = aMkSingletonStrategy (UnMark p)

iUnMark :: Pheromone -> AntImperative
iUnMark p = Single $ CUnMark p


aDrop :: AntStrategy
aDrop = aMkSingletonStrategy Drop

iDrop :: AntImperative
iDrop = Single $ CDrop


aTurn :: Dexterity -> AntStrategy
aTurn d = aMkSingletonStrategy (Turn d)

iTurn :: Dexterity -> AntImperative
iTurn d = Single $ CTurn d


aTurnL :: AntStrategy
aTurnL = aTurn L

iTurnL :: AntImperative
iTurnL = iTurn L


aTurnR :: AntStrategy
aTurnR = aTurn R

iTurnR :: AntImperative
iTurnR = iTurn R



-- | This function defines the semantics of the AntImperative deep-embedded EDSL
semanticsBasic :: AntBasic -> AntStrategy
semanticsBasic (CMark p)   = aMark p
semanticsBasic (CUnMark p) = aUnMark p
semanticsBasic CDrop       = aDrop
semanticsBasic (CTurn d)   = aTurn d

