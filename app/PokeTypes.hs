module PokeTypes where


import Data.List

-- a module for the different types of information we can find about a pokemon
-- the MissingNo constructor represents a failure to retreive data. it is also a cute joke.
data PokeData = MoveList [String] | AbilitySet [String] | TypeCombo String | StatDistribution [String] | MissingNo

instance Show PokeData where
    show (MoveList   moves)       = unlines moves
    show (AbilitySet abilities)   = unlines abilities
    show (TypeCombo  types)       = types
    show (StatDistribution stats) = unlines stats
    show MissingNo                = "Error MissingNo"
 
 

