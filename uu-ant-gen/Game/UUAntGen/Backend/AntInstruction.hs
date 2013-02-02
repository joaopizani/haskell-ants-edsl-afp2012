module Game.UUAntGen.Backend.AntInstruction where

import           Control.Monad.Supply (Supply, supply)
import           Control.Monad.State  (StateT,put)
import           Control.Monad.Reader (ReaderT,ask)
import           Control.Monad.Trans  (lift)
import qualified Data.Map             as M

import Game.UUAntGen.Backend.AntAssembly
import Game.UUAntGen.Backend.AntDeepEmbedded


type IMap = M.Map AntState AntInstruction

-- | The base type that includes a map, an initial state, a set of final states and a IMap
type AntStrategy' = (AntState,[AntState],IMap)

-- | The AntStrategy' type, wrapped in the Supply monad for obtaining unique instruction ids and wrapped into a Reader monad that supplies the next state available.
type AntStrategy = ReaderT AntState (Supply AntState) AntStrategy' 


-- Basic strategies, one function per assembly word. Each basic strategy has a "real assembly"
-- version (of type AntStrategy), and a "deep embedded" EDSL version (of type AntImperative).
-- The conversion AntBasic -> AntStrategy is done by the semanticsBasic function
aMkSingletonStrategy' :: (AntState -> AntInstruction) -> AntState -> AntState
                      -> AntStrategy' 
aMkSingletonStrategy' instr idx ns = (idx, [idx], M.fromList [(idx, instr ns)])


aMkSingletonStrategy :: (AntState -> AntInstruction) -> AntStrategy
aMkSingletonStrategy instr = do 
    idx <- supply
    nextState <- ask
    return $ aMkSingletonStrategy' instr idx nextState 


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

