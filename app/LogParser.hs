module LogParser where
-- functions for parsing data from a log string

import Data.Char
import Data.List
import Data.Maybe

import PokeTypes

-- main retrievers
getPokemon :: String -> [String]
getPokemon logStr = map (grabName) infoLines
    where infoLines  = (takeWhile (not . null) . drop 6) (lines logStr)
          grabName l =  (takeWhile (/=' ')) (drop 4 l)
-- gets the basic information of a pokemon, given a log string
-- this holds their type, base stats, abilities, and held items
getInfoLine :: String -> String -> Maybe String
getInfoLine logStr name = find (matchesName) (lines logStr)
    where matchesName l = ('|':name) `isPrefixOf` (drop 3 l)
 
-- gets full info of a pokemon given a log string
-- full info starts with "#ID {NAME} -> {EVO}"
-- and ends with an empty line 
-- this holds their (main) evolution, base states, and level-up learnlist
getInfo :: String -> String -> Maybe [String]
getInfo logStr name
    | null infoChains = Nothing
    | otherwise = Just (head $ infoChains)
    where infoGroups = groupBy(\i j -> startOfInfo i && (not . null) j) (lines logStr)
          infoChains = filter (\x -> length x > 1) infoGroups
          startOfInfo l = (name ++ " ->") `isPrefixOf` (drop 4 l)
          
-- machines and tutor moves are formatted similarly - might combine these         
-- gets tm compatibility string of a pokemon
getTMCompatibility :: String -> String -> Maybe String
getTMCompatibility logStr name = find (matchesName) (tmData)
    where tmData        = dropWhile (/= "--TM Compatibility--") (lines logStr)
          matchesName l = (' ':name) `isPrefixOf` (drop 3 l)

-- gets tutor compatibility string of a pokemon
getTutorCompatibility :: String -> String -> Maybe String
getTutorCompatibility logStr name = find (matchesName) (tutorData)
    where tutorData     = dropWhile (/= "--Move Tutor Compatibility--") (lines logStr)
          matchesName l = (' ':name) `isPrefixOf` (drop 3 l)


----------------------------------------------------------------------------
-- the functions below wrap the strings gathered above in pertinent types --
----------------------------------------------------------------------------


getBaseStats :: String -> String -> PokeData
getBaseStats logStr name = case (getInfo logStr name) of
    Just info  -> StatDistribution $ (take 6 . drop 1) info
    Nothing    -> MissingNo

{-- deprecated with move to PokeData
getBaseStatTotal :: String -> String -> Maybe Int
getBaseStatTotal logStr name = fmap (sumStats) (getBaseStats logStr name)
        where sumStats = sum . map (read . last . words)
--}
        

getType :: String -> String -> PokeData
getType logStr name = case (getInfoLine logStr name) of
    Just infoLine -> TypeCombo $ (drop 1 . head . drop 1 . words) infoLine
    Nothing       -> MissingNo
    


getAbilities :: String -> String -> PokeData
getAbilities logStr name = case (getInfoLine logStr name) of
    Just infoLine -> AbilitySet $ (take 3 . drop 9 . splitByBars) infoLine
    Nothing       -> MissingNo
    
getLevelUpMoves :: String -> String -> PokeData
getLevelUpMoves logStr name = case (getInfo logStr name) of
    Just info    -> MoveList $ filter (isPrefixOf "Level") info
    Nothing      -> MissingNo
    

getMachines :: String -> String -> PokeData
getMachines logStr name = case (getTMCompatibility logStr name) of
    Just tmCompatibility -> MoveList $ (filterMachines . splitByBars) tmCompatibility
        where filterMachines = filter (\x -> "HM" `isPrefixOf` x || "TM" `isPrefixOf` x)
    Nothing              -> MissingNo
        

getTutorMoves :: String -> String -> PokeData
getTutorMoves logStr name = case (getTutorCompatibility logStr name) of
    Just tutorCompatibility -> MoveList $ (filterMoves . drop 1 . splitByBars) tutorCompatibility
        where filterMoves = filter (notElem '-')
    Nothing                 -> MissingNo

-- sub info 


-- functions for searching
-- the logs have some information stored in upper or title case
titleCase :: String -> String
titleCase xs = unwords $ map titleWord (words xs)
    where titleWord w = toTitle(head w):(map toLower $ tail w)

upperCase :: String -> String
upperCase = map toUpper

-- given a string seperated by | characters, gives the substrings inbetween
splitByBars :: String -> [String]
splitByBars = (filter (/="|")) . (groupBy (\x y -> x /= '|' && y /= '|'))