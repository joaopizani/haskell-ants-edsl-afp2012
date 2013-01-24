module Game.UUAntGen.Test.AntInstructionQuickCheck where

import Test.QuickCheck      (Gen(..),Arbitrary(..),arbitrary,Property,oneof,forAll
                            ,listOf,choose,sized,frequency,vectorOf,quickCheck)
import Control.Monad        (liftM,liftM2,liftM3)
import Data.Map             ((!),elems,size,keys)
import Data.Set             (fromList,isSubsetOf,delete)
import Data.List            (nub)
import Control.Monad.Supply

import Game.UUAntGen.Test.AntInstructionArbitrary
import Game.UUAntGen.AntDeepEmbedded
import Game.UUAntGen.AntInstruction
import Game.UUAntGen.AntTransformation
import Game.UUAntGen.AntBasic
import Game.UUAntGen.AntImperative

-- Generators

actions :: Gen Imperative
actions = liftM IList $ liftM2 (:) arbitrary (listOf arbitrary)

-- FIXME: this should be modified
actionsGhost :: Gen Imperative 
actionsGhost = liftM IList $ (listOf arbitrary) `catM` 
                             (singlM (liftM Single arbitrary))
    where catM   = liftM2 (++)
          singlM = liftM  (:[])

singleAction :: Gen Imperative 
singleAction = liftM Single arbitrary

instance Arbitrary Imperative where
    arbitrary = sized genImperative 

genImperative :: Int -> Gen Imperative
genImperative 0 = liftM Single arbitrary 
genImperative n = frequency $ 
                  [ (5, liftM  Single arbitrary)
                  , (1, liftM3 IfThenElse arbitrary (genImperative (n `div` 2)) 
                                                    (genImperative (n `div` 2)))
                  , (1, liftM2 While arbitrary (genImperative (n `div` 2))) ]
    where genActs n = choose (1,4) >>= 
                      \l -> vectorOf l (genImperative (n `div` 2))
 
instance Arbitrary Basic where
    arbitrary = oneof [ liftM CMark arbitrary 
                      , liftM CUnMark arbitrary 
                      , return CDrop
                      , liftM CTurn arbitrary ]
 
instance Arbitrary AntTest where
    arbitrary = oneof [ return TryForward
                      , return TryPickUp
                      , liftM2 TrySense arbitrary arbitrary
                      , liftM  TryRandomEqZero (choose (0,maxBound)) ]

-- The actual properties

mkAntStrategyProp :: (AntStrategy' -> Bool) -> Gen Imperative -> Property
mkAntStrategyProp p actions = forAll actions $ \as ->
        let as' = semanticsImperative as 
         in p $ fst $ runSupply as' [AntState 0..]

finalRefsItselfProp :: Property
finalRefsItselfProp = mkAntStrategyProp finalRefsItself actions 

singleInstructionProp :: Property
singleInstructionProp = mkAntStrategyProp refsItself singleAction 

noBrokenRefsProp :: Property 
noBrokenRefsProp = mkAntStrategyProp noBrokenRefs actionsGhost -- FIXME: ghost gen 

noGhostsAfterBustingProp :: Property
noGhostsAfterBustingProp = mkAntStrategyProp noGhostsAfterBusting actionsGhost

initialKeyIsZeroProp :: Property
initialKeyIsZeroProp = mkAntStrategyProp initialKeyIsZero actionsGhost

keySpaceTransformProp :: Property
keySpaceTransformProp = mkAntStrategyProp keySpaceTransform actionsGhost

propList = [ ("Final references itself..."             , finalRefsItselfProp)
           , ("Single instructions point themselves...", singleInstructionProp)
           , ("No broken references..."                , noBrokenRefsProp)
           , ("No ghosts after busting..."             , noGhostsAfterBustingProp)
           , ("Initial key is zero..."                 , initialKeyIsZeroProp)
           , ("Continuous key space transform..."      , keySpaceTransformProp) ]


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
    let m    = fromKeysToLineNumbers $ ghostBuster $ as
        k    = size m 
        setM = fromList $ map toInt $ concatMap getAntStates $ elems m
        set  = fromList $ map toInt $ keys m 
     in setM `isSubsetOf` set 
    where toInt (AntState s) = s

-- | There are no ghosts left after they were busted
noGhostsAfterBusting :: AntStrategy' -> Bool
noGhostsAfterBusting as = null $ filter isGhost $ elems $ instructions $ 
                          ghostBuster $ as
    where isGhost (Ghost _ _ _) = True 
          isGhost _             = False
       
-- | Ensures the initial key is zero after running fromKeysToLineNumbers 
initialKeyIsZero :: AntStrategy' -> Bool
initialKeyIsZero as = 
    let m = fromKeysToLineNumbers $ ghostBuster as 
     in head (keys m) == (AntState 0)

-- | Check whether fromKeysToLineNumbers transforms keys from a range 0..n-1
keySpaceTransform :: AntStrategy' -> Bool
keySpaceTransform as =
    let m  = fromKeysToLineNumbers $ ghostBuster $ as
        ks = nub $ map toInt $ keys m
     in length ks == (maximum ks) + 1 && minimum ks == 0 
    where toInt (AntState s) = s 


-- QuickCheck helper functions

-- | QuickCheck all properties!
quickCheckAll = sequence_ $ map process propList 
    where process (x,y) = putStrLn x >> quickCheck y

