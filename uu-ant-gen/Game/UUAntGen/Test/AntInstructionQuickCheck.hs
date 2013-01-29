module Game.UUAntGen.Test.AntInstructionQuickCheck where

import Control.Monad        (liftM, liftM2, liftM3)
import Control.Monad.Supply
import Data.List            (nub)
import Data.Map             (elems, keys, size, (!))
import Data.Set             (delete, fromList, isSubsetOf)
import Test.QuickCheck      (Arbitrary (..), Gen (..), Property, arbitrary, choose, forAll,
                             frequency, listOf, oneof, quickCheck, sized, vectorOf)

import Game.UUAntGen.AntAssembly
import Game.UUAntGen.AntInstruction
import Game.UUAntGen.AntDeepEmbedded
import Game.UUAntGen.AntImperative
import Game.UUAntGen.AntTransformation
import Game.UUAntGen.Test.AntInstructionArbitrary


-- Generators
instance Arbitrary AntBasic where
    arbitrary = oneof $
        [ liftM  CMark   arbitrary
        , liftM  CUnMark arbitrary
        , return CDrop
        , liftM  CTurn   arbitrary ]

singleAction :: Gen AntImperative
singleAction = liftM Single arbitrary


instance Arbitrary AntTest where
    arbitrary = oneof $
        [ return TryForward
        , return TryPickUp
        , liftM2 TrySense arbitrary arbitrary
        , liftM  TryRandomEqZero (choose (0,maxBound)) ]

instance Arbitrary AntImperative where
    arbitrary = sized genImperative

genImperative :: Int -> Gen AntImperative
genImperative 0 = liftM Single arbitrary
genImperative n = frequency $
    [ (5, liftM  Single     arbitrary)
    , (5, liftM  SideEffect arbitrary)
    , (1, liftM3 IfThenElse arbitrary (genImperative (n `div` 2)) (genImperative (n `div` 2)))
    , (1, liftM2 IfThen     arbitrary (genImperative (n `div` 2)))
    , (1, liftM2 While      arbitrary (genImperative (n `div` 2)))
    , (4, liftM  IList      (genImps n)) ]
    where genImps n = choose (1,4) >>= \l -> vectorOf l (genImperative (n `div` 2))

actions :: Gen AntImperative
actions = liftM IList $ liftM2 (:) arbitrary (listOf arbitrary)



-- The actual properties
mkAntStrategyProp :: (AntStrategy' -> Bool) -> Gen AntImperative -> Property
mkAntStrategyProp p actions = forAll actions $ \as ->
        let as' = semanticsImp as
         in p $ aForever $ fst $ runSupply as' [AntState 0..]


finalRefsItselfProp :: Property
finalRefsItselfProp = mkAntStrategyProp finalRefsItself actions


singleInstructionProp :: Property
singleInstructionProp = mkAntStrategyProp refsItself singleAction


noBrokenRefsProp :: Property
noBrokenRefsProp = mkAntStrategyProp noBrokenRefs actions


noGhostsAfterBustingProp :: Property
noGhostsAfterBustingProp = mkAntStrategyProp noGhostsAfterBusting actions


initialKeyIsZeroProp :: Property
initialKeyIsZeroProp = mkAntStrategyProp initialKeyIsZero actions


keySpaceTransformProp :: Property
keySpaceTransformProp = mkAntStrategyProp keySpaceTransform actions


propList = 
    [ ("Final references itself..."             , finalRefsItselfProp)
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
noBrokenRefs as = setM `isSubsetOf` set
    where
        m    = keysToLineNumbers $ ghostBuster $ as
        k    = size m
        setM = fromList $ map toInt $ concatMap getAntStates $ elems m
        set  = fromList $ map toInt $ keys m
        toInt (AntState s) = s


-- | There are no ghosts left after they were busted
noGhostsAfterBusting :: AntStrategy' -> Bool
noGhostsAfterBusting = null . filter isGhost . elems . instructions . ghostBuster
    where isGhost (Ghost _ _) = True
          isGhost _           = False


-- | Ensures the initial key is zero after running keysToLineNumbers
initialKeyIsZero :: AntStrategy' -> Bool
initialKeyIsZero as =
    let m = keysToLineNumbers $ ghostBuster as
     in head (keys m) == (AntState 0)


-- | Check whether keysToLineNumbers transforms keys from a range 0..n-1
keySpaceTransform :: AntStrategy' -> Bool
keySpaceTransform as =
    let m  = keysToLineNumbers $ ghostBuster $ as
        ks = nub $ map toInt $ keys m
     in length ks == (maximum ks) + 1 && minimum ks == 0
    where toInt (AntState s) = s



-- | QuickCheck all properties!
quickCheckAll = mapM_ (\(name, p) -> putStrLn name >> quickCheck p) propList

