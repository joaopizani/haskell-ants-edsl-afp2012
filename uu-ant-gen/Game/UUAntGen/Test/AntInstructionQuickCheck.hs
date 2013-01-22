module Game.UUAntGen.Test.AntInstructionQuickCheck where

import Test.QuickCheck   (Gen(..),arbitrary,Property,oneof,forAll)
import Control.Monad     (liftM)
import Data.HashMap.Lazy ((!))

import Game.UUAntGen.Test.AntInstructionArbitrary
import Game.UUAntGen.AntMap
import Game.UUAntGen.AntInstruction

singleInstructionGen :: Gen AntStrategy'
singleInstructionGen = oneof [ liftM aMark' arbitrary 
                             , liftM aUnMark' arbitrary
                             , return aDrop'
                             , return aTurnL' 
                             , return aTurnR'
                             ]

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
    where refsItself :: AntStrategy' -> Bool
          refsItself as = let ins  = instructions as
                              init = initial as
                           in init == final as &&
                              getDefaultState (ins ! init) == init

getDefaultState (Sense d _ s c) = s 
getDefaultState (Mark _ s)      = s
getDefaultState (UnMark _ s)    = s
getDefaultState (PickUp _ s)    = s 
getDefaultState (Drop s)        = s
getDefaultState (Turn _ s)      = s
getDefaultState (Move _ s)      = s
getDefaultState (Flip i s _)    = s


