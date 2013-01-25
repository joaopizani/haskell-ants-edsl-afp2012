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
    | Not AntTest
    | TryForward
    | TryPickUp
    deriving (Eq, Show)


data AntImperative
    = Single AntBasic
    | IfThenElse AntTest AntImperative AntImperative
    | IfThen AntTest AntImperative
    | While AntTest AntImperative
    | SideEffect AntTest
    | IList [AntImperative]
    deriving (Eq, Show)

