{-# LANGUAGE FlexibleInstances #-}
module Game.UUAntGen.Test.AntInstructionQuickCheck where

import Test.QuickCheck.Monadic
import Test.QuickCheck   (Gen(..),Arbitrary(..),arbitrary,Property,oneof,forAll
                         ,listOf,choose,sized,frequency,vectorOf)
import Control.Monad     (liftM,liftM2,liftM3)
import Data.Map          ((!),elems,size,keys)
import Data.Set          (fromList,isSubsetOf,delete)


import Control.Monad.Supply

import Game.UUAntGen.Test.AntInstructionArbitrary
import Game.UUAntGen.AntMap 
import Game.UUAntGen.AntInstruction

import Debug.Trace

data Action 
    = Single Command
    | If AntTest [Action] [Action] 
    | While AntTest [Action] 
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
      If c t e           -> as >>- semantics (aIfThenElse c (perform t) (perform e)) 
                                             tl 
      While c b          -> as >>- semantics (aWhile c (perform b)) tl  

perform :: [Action] -> AntStrategy
perform []     = aDrop -- this should be working on non-empty lists...
perform (a:tl) = 
    case a of
      Single (CMark p)   -> semantics (aMark p) tl 
      Single (CUnMark p) -> semantics (aUnMark p) tl 
      Single CDrop       -> semantics aDrop tl
      Single (CTurn d)   -> semantics (aTurn d) tl
      If c t e           -> aIfThenElse c (perform t) (perform e) 
      While c b          -> aWhile c (perform b) 


-- Generators

actions :: Gen [Action]
actions = liftM2 (:) arbitrary (listOf arbitrary)

-- FIXME: this should be modified
actionsGhost :: Gen [Action]
actionsGhost = (listOf arbitrary) `catM` (singlM (liftM Single arbitrary))
    where catM   = liftM2 (++)
          singlM = liftM  (:[])

singleAction :: Gen [Action]
singleAction = liftM (\x -> [Single x]) arbitrary

instance Arbitrary Action where
    arbitrary = sized genAction 
        where 
              genAction 0 = liftM Single arbitrary 
              genAction n = frequency $ 
                                [ (5, liftM  Single arbitrary)
                                , (1, liftM3 If arbitrary (genActs n) (genActs n))
                                , (1, liftM2 While arbitrary (genActs n)) ]
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

-- The actual properties

mkAntStrategyProp :: (AntStrategy' -> Bool) -> Gen [Action] -> Property
mkAntStrategyProp p actions = forAll actions $ \as ->
        let as' = perform as 
         in p $ fst $ runSupply as' [AntState 0..]

finalRefsItselfProp :: Property
finalRefsItselfProp = mkAntStrategyProp finalRefsItself actions 

singleInstructionProp :: Property
singleInstructionProp = mkAntStrategyProp refsItself singleAction 

noBrokenRefsProp :: Property 
noBrokenRefsProp = mkAntStrategyProp noBrokenRefs actionsGhost -- FIXME: ghost gen 


-- AntStrategy' predicates

-- | A single instruction should reference itself
refsItself :: AntStrategy' -> Bool
refsItself (AntStrategy' m i f) = i == f && getDefaultState (m ! i) == i

-- | The last instruction should reference itself
finalRefsItself :: AntStrategy' -> Bool
finalRefsItself (AntStrategy' m i f) = getDefaultState (m ! f) == f

-- | There are no broken references (right after creating the AntStrategy') 
noBrokenRefs :: AntStrategy' -> Bool
noBrokenRefs as = 
    let (AntStrategy' m i f) = fromKeysToLineNumbers $ ghostBuster $ as
        k    = size m 
        setM = fromList $ map toInt $ concatMap getAntStates $ elems m
        set  = fromList $ map toInt $ keys m 
     in setM `isSubsetOf` set 
    where toInt (AntState s) = s

