{-
 - Our approach is base in the transformation of a High-Level representation of an ant's
 - strategy (a deep-embedded DSL) to a low-level representation. The types related to this
 - deep EDSL are defined in this module.
 -}
module Game.UUAntGen.Backend.AntDeepEmbedded where

import Game.UUAntGen.Backend.AntAssembly


-- | Datatype representing unconditional instructions
data AntBasic
    = CMark Pheromone
    | CUnMark Pheromone
    | CDrop
    | CTurn Dexterity 
    deriving (Eq, Show)


-- | Datatype representing all the possible tests to be performed in a conditional AntStrategy
data AntTest
    = TrySense Direction Condition
    | TryRandomEqZero Int
    | TryForward
    | TryPickUp
    | And AntTest AntTest
    | Or AntTest AntTest
    | Not AntTest
    deriving (Eq, Show)


-- | An algebra for AntTest. A tuple of functions returning a type r, one for each constructor
type AntTestAlgebra r
    = ( Direction -> Condition -> r  -- semantics for TrySense
      , Int -> r  -- semantics for TryRandomEqZero
      , r  -- semantics for TryForward
      , r  -- semantics for TryPickUp
      , r -> r -> r  -- semantics for And
      , r -> r -> r  -- semantics for Or
      , r -> r )  -- semantics for Not


-- | Given an algebra, transforms a value of type AntTest to type r. Will be mainly used
-- to transform an AntTest (recursive type) into a AntStrategy
foldAntTest :: AntTestAlgebra r -> AntTest -> r
foldAntTest (sense', random', forward', pickup', and', or', not') = fold'
    where fold' (TrySense d c)      = sense' d c
          fold' (TryRandomEqZero p) = random' p
          fold' TryForward          = forward'
          fold' TryPickUp           = pickup'
          fold' (And t1 t2)         = and' (fold' t1) (fold' t2)
          fold' (Or t1 t2)          = or' (fold' t1) (fold' t2)
          fold' (Not t)             = not' (fold' t)


-- | This is the high-level datatype over which all functions in our EDSL operate. A semantics is
-- defined which converts from values of this type to the low-level AntStrategy
data AntImperative
    = Single AntBasic
    | IfThenElse AntTest AntImperative AntImperative
    | IfThen AntTest AntImperative
    | While AntTest AntImperative
    | SideEffect AntTest
    | Case [(AntTest,AntImperative)]
    | IList [AntImperative]
    deriving (Eq, Show)

