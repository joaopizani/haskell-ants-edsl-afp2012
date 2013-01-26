module Game.UUAntGen.AntDeepEmbedded where

import Game.UUAntGen.AntAssembly


data AntBasic
    = CMark Pheromone
    | CUnMark Pheromone
    | CDrop
    | CTurn Dexterity 
    deriving (Eq, Show)


-- Datatype representing all the possible tests to be performed in a conditional AntStrategy
-- TODO we want to join conditions (OR, AND)
data AntTest
    = TrySense Direction Condition
    | TryRandomEqZero Int
    | TryForward
    | TryPickUp
    | And AntTest AntTest
    | Not AntTest
    deriving (Eq, Show)

type AntTestAlgebra r = ( Direction -> Condition -> r
                        , Int -> r
                        , r
                        , r
                        , r -> r -> r
                        , r -> r )

foldAntTest :: AntTestAlgebra r -> AntTest -> r
foldAntTest (sense,random,forward,pickup,and,not) = fold
    where fold (TrySense d c)      = sense d c
          fold (TryRandomEqZero p) = random p 
          fold TryForward          = forward
          fold TryPickUp           = pickup
          fold (And t1 t2)         = and (fold t1) (fold t2)
          fold (Not t)             = not (fold t)


data AntImperative
    = Single AntBasic
    | IfThenElse AntTest AntImperative AntImperative
    | IfThen AntTest AntImperative
    | While AntTest AntImperative
    | SideEffect AntTest
    | IList [AntImperative]
    deriving (Eq, Show)

