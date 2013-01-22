module Game.UUAntGen.Test.AntInstructionQuickCheck where

import Test.QuickCheck.Monadic
import Test.QuickCheck   (Gen(..),Arbitrary(..),arbitrary,elements,Property
                         ,oneof,forAll,listOf)
import Control.Monad     (liftM,liftM2)
import Data.Map          ((!))

import Control.Monad.Supply

import Game.UUAntGen.Test.AntInstructionArbitrary
import Game.UUAntGen.AntMap
import Game.UUAntGen.AntInstruction

--PropertyM (Supply AntState) AntStrategy' 

data Action  = Single Command
    deriving Show
data Command = CDrop
             | CTurn Dexterity 
    deriving Show


semantics :: AntStrategy -> [Action] -> AntStrategy
semantics as []     = as
semantics as (a:tl) =
    case a of
      Single CDrop     -> as >>- (semantics aDrop tl)
      Single (CTurn d) -> as >>- (semantics (aTurn d) tl)

perform :: [Action] -> AntStrategy
perform []     = aDrop -- this should be working on non-empty lists...
perform (a:tl) = 
    case a of
      Single CDrop     -> semantics aDrop tl
      Single (CTurn d) -> semantics (aTurn d) tl

actions :: Gen [Action]
actions = liftM2 (:) arbitrary (listOf arbitrary)

instance Arbitrary Action where
    arbitrary = liftM Single arbitrary

instance Arbitrary Command where
    arbitrary = elements [CDrop,CTurn L,CTurn R]

supplyProp :: PropertyM (Supply AntState) a -> Property
supplyProp p = monadic f p
    where f :: Supply AntState Property -> Property
          f s = fst $ runSupply s [AntState 0..]

finalRefsItselfProp :: Property
finalRefsItselfProp = supplyProp finalRefsItselfPropM

finalRefsItselfPropM :: PropertyM (Supply AntState) ()
finalRefsItselfPropM = forAllM actions $ \as -> do
    b <- run (perform as)
    assert (lastRefsItself b)
    return ()

singleInstructionProp :: Property
singleInstructionProp = supplyProp singleInstructionPropM 

singleInstructionPropM :: PropertyM (Supply AntState) () 
singleInstructionPropM = do
    b <- run aDrop -- FIXME: add more actions
    assert (refsItself b)


singleInstructionGen :: Gen AntStrategy'
singleInstructionGen = oneof [ liftM aMark' arbitrary 
                             , liftM aUnMark' arbitrary
                             , return aDrop'
                             , return aTurnL' 
                             , return aTurnR'
                             ]

genTest :: Gen (AntStrategy)
genTest = return aDrop

aMark' :: Pheromone -> AntStrategy'
aMark' p = aMkSingletonStrategy' (Mark p) (AntState 0)

aUnMark' :: Pheromone -> AntStrategy'
aUnMark' p = aMkSingletonStrategy' (UnMark p) (AntState 0)

aDrop' :: AntStrategy'
aDrop' = aMkSingletonStrategy' Drop (AntState 0)

aTurn' :: Dexterity -> AntStrategy'
aTurn' d = aMkSingletonStrategy' (Turn d) (AntState 0)

aTurnL' :: AntStrategy'
aTurnL' = aTurn' L

aTurnR' :: AntStrategy'
aTurnR' = aTurn' R

singleInstructionFinal :: Property
singleInstructionFinal = forAll singleInstructionGen refsItself

refsItself :: AntStrategy' -> Bool
refsItself as = let ins  = instructions as
                    init = initial as
                    fin  = final as
                 in init == fin &&
                    getDefaultState (ins ! init) == init

lastRefsItself :: AntStrategy' -> Bool
lastRefsItself as = let ins  = instructions as
                        fin  = final as
                     in getDefaultState (ins ! fin) == fin

getDefaultState (Sense d _ s c) = s 
getDefaultState (Mark _ s)      = s
getDefaultState (UnMark _ s)    = s
getDefaultState (PickUp _ s)    = s 
getDefaultState (Drop s)        = s
getDefaultState (Turn _ s)      = s
getDefaultState (Move _ s)      = s
getDefaultState (Flip i s _)    = s


