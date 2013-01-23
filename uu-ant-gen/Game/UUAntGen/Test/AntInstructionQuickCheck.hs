{-# LANGUAGE FlexibleInstances #-}
module Game.UUAntGen.Test.AntInstructionQuickCheck where

import Test.QuickCheck.Monadic
import Test.QuickCheck   (Gen(..),Arbitrary(..),arbitrary,Property,oneof,forAll
                         ,listOf,choose,sized,frequency,vectorOf)
import Control.Monad     (liftM,liftM2,liftM3)
import Data.Map          ((!))

import Control.Monad.Supply

import Game.UUAntGen.Test.AntInstructionArbitrary
import Game.UUAntGen.AntMap
import Game.UUAntGen.AntInstruction

--PropertyM (Supply AntState) AntStrategy' 

data Action 
    = Single Command
    | If AntTest [Action] [Action] 
    | While AntTest [Action] Action
    deriving Show

data Command 
    = CMark Pheromone
    | CUnMark Pheromone
    | CDrop
    | CTurn Dexterity 
    deriving Show


semantics :: AntStrategy -> [Action] -> AntStrategy
semantics as []     = as
semantics as (a:tl) =
    case a of
      Single (CMark p)   -> as >>- semantics (aMark p) tl 
      Single (CUnMark p) -> as >>- semantics (aUnMark p) tl 
      Single CDrop       -> as >>- semantics aDrop tl
      Single (CTurn d)   -> as >>- semantics (aTurn d) tl
      If c t e           -> as >>- aIfThenElse c (perform t) (perform e) 
      While c b a'       -> as >>- aWhile c (perform b) (perform (a':tl)) 

perform :: [Action] -> AntStrategy
perform []     = aDrop -- this should be working on non-empty lists...
perform (a:tl) = 
    case a of
      Single (CMark p)   -> semantics (aMark p) tl 
      Single (CUnMark p) -> semantics (aUnMark p) tl 
      Single CDrop       -> semantics aDrop tl
      Single (CTurn d)   -> semantics (aTurn d) tl
      If c t e           -> aIfThenElse c (perform t) (perform e) 
      While c b a'       -> aWhile c (perform b) (perform (a':tl)) 

actions :: Gen [Action]
actions = liftM2 (:) arbitrary (listOf arbitrary)

singleAction :: Gen [Action]
singleAction = liftM (\x -> [Single x]) arbitrary

instance Arbitrary Action where
    arbitrary = sized genAction 
        where 
              genAction 0 = liftM Single arbitrary 
              genAction n = frequency $ 
                                [ (5, liftM  Single arbitrary)
                                , (1, liftM3 If arbitrary (genActs n) (genActs n))
                                , (1, liftM3 While arbitrary (genActs n) 
                                                             (genAction 1)) ]
              genActs n = choose (1,4) >>= \l -> vectorOf l (genAction (n `div` 2))

instance Arbitrary Command where
    arbitrary = oneof [ liftM CMark arbitrary 
                      , liftM CUnMark arbitrary 
                      , return CDrop
                      , liftM CTurn arbitrary ]
 
instance Arbitrary AntTest where
    arbitrary = oneof [ return TryForward
                      , return TryPickup
                      , liftM2 TrySense arbitrary arbitrary
                      , liftM  TryRandomEqZero (choose (0,maxBound)) ]

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

singleInstructionProp :: Property
singleInstructionProp = supplyProp singleInstructionPropM 

singleInstructionPropM :: PropertyM (Supply AntState) () 
singleInstructionPropM = forAllM singleAction $ \a -> do
    b <- run (perform a) 
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

getDefaultState :: AntInstruction -> AntState
getDefaultState (Sense d _ s c) = s 
getDefaultState (Mark _ s)      = s
getDefaultState (UnMark _ s)    = s
getDefaultState (PickUp _ s)    = s 
getDefaultState (Drop s)        = s
getDefaultState (Turn _ s)      = s
getDefaultState (Move _ s)      = s
getDefaultState (Flip i s _)    = s
getDefaultState (Ghost s _ _)   = s


