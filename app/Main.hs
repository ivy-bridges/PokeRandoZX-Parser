import Data.Char
import Data.List
import Data.Maybe

import Control.Monad

import System.IO (hFlush, stdin, stdout)

import LogParser



main :: IO ()
main = do
    putStrLn "unfinished"
    
-- asks for an item and cuts down the list until a small number are left to choose. it. works right now.
-- TODO: Rewrite this to not use a bunch of else-ifs lol (something at the level of pokeLoop would be nice)
chooseItem :: (Show a) => [a] -> (String -> a -> Bool) -> IO a
chooseItem options chooseFunction = do
    hFlush stdout
    input <- getLine
    let left = filter (chooseFunction input) options
    if null left then do
        putStrLn "None of the options matched your choice."
        chooseItem options chooseFunction
    else if length left == 1 then do
        return (head left)
    else if length left < 10 then do
        zipWithM_ (\e i -> putStrLn(show i ++ ". " ++ show e)) left [1..(length left)]
        index <- getChar
        getLine
        if (isDigit index && (digitToInt index < (length left + 1))) then do
            return (left !! ((digitToInt index - 1)))
        else do
            chooseItem left chooseFunction
    else do
        putStrLn "This did not filter enough elements."
        chooseItem options chooseFunction
        

-- main loops
infoLoop :: String -> IO ()
infoLoop logText = do
    let pokeNames = getPokemon logText
    
    putStrLn "choose a pokemon"
    
    item <- chooseItem pokeNames (isPrefixOf . titleCase)
    putStrLn $ "Showing info on [" ++ (upperCase item) ++ "]"
    pokeLoop logText item
    infoLoop logText
    
pokeLoop :: String -> String -> IO ()
pokeLoop logStr pokemon = do
    let options = [("STATS",     getBaseStats    logStr pokemon), 
                   ("TYPE",      getType         logStr pokemon), 
                   ("MOVES",     getLevelUpMoves logStr pokemon),  
                   ("ABILITIES", getAbilities    logStr pokemon),  
                   ("MACHINES",  getMachines     logStr pokemon),
                   ("TUTOR",     getTutorMoves   logStr pokemon)]
    input <- getLine
    
    let isQuitting = elem (upperCase input) ["Q","C","QUIT","CLOSE","BACK"]
    unless isQuitting (do -- requests for data loop back after displaying
        let match   = find  (isPrefixOf (upperCase input) . fst) options -- get the first pair where the input prefixes the string 
            result  = liftM (show . snd) match -- representation of the data (or Nothing on an invalid match)
        maybe (putStrLn "Invalid Command") putStrLn result
        pokeLoop logStr pokemon)
    
    infoLoop logStr
    


    