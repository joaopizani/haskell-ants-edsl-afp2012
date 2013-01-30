module Game.UUAntGen.Backend.AntDeepEmbedded where

import Game.UUAntGen.Backend.AntAssembly


data AntBasic
    = CMark Pheromone
    | CUnMark Pheromone
    | CDrop
    | CTurn Dexterity 
    deriving (Eq, Show)


-- Datatype representing all the possible tests to be performed in a conditional AntStrategy
data AntTest
    = TrySense Direction Condition
    | TryRandomEqZero Int
    | TryForward
    | TryPickUp
    | And AntTest AntTest
    | Or AntTest AntTest
    | Not AntTest
    deriving (Eq, Show)

type AntTestAlgebra r
    = ( Direction -> Condition -> r  -- semantics for TrySense
      , Int -> r  -- semantics for TryRandomEqZero
      , r  -- semantics for TryForward
      , r  -- semantics for TryPickUp
      , r -> r -> r  -- semantics for And
      , r -> r -> r  -- semantics for Or
      , r -> r )  -- semantics for Not

foldAntTest :: AntTestAlgebra r -> AntTest -> r
foldAntTest (sense', random', forward', pickup', and', or', not') = fold'
    where fold' (TrySense d c)      = sense' d c
          fold' (TryRandomEqZero p) = random' p
          fold' TryForward          = forward'
          fold' TryPickUp           = pickup'
          fold' (And t1 t2)         = and' (fold' t1) (fold' t2)
          fold' (Or t1 t2)          = or' (fold' t1) (fold' t2)
          fold' (Not t)             = not' (fold' t)


data AntImperative
    = Single AntBasic
    | IfThenElse AntTest AntImperative AntImperative
    | IfThen AntTest AntImperative
    | While AntTest AntImperative
    | SideEffect AntTest
    | Case [(AntTest,AntImperative)]
    | IList [AntImperative]
    deriving (Eq, Show)

