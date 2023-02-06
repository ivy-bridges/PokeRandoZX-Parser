import Data.Char
import Data.List
import Data.Maybe

import Control.Monad

import System.IO (hFlush, stdin, stdout)

import LogParser



main :: IO ()
main = do
    putStrLn "Please enter the name of the log file."
    
    inputFile <- getLine
    logText   <- readFile inputFile
    
    putStrLn "After choosing a Pokemon, type any of the following to display the relevant info."
    putStrLn "\tStats\n\tTypes\n\tMoves\n\tAbilities\n\tMachines\n\tTutors"
    putStrLn "Or type Quit, Close, or Back to exit and choose another Pokemon."
    infoLoop logText
    
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
        putStrLn "Enter the corresponding number of your selection:"
        zipWithM_ (\e i -> putStrLn(show i ++ ". " ++ show e)) left [1..(length left)]
        index <- getChar
        getLine
        if (isDigit index && (digitToInt index < (length left + 1))) then do
            return (left !! ((digitToInt index - 1)))
        else do
            chooseItem options chooseFunction
    else do
        putStrLn "This did not filter enough elements."
        chooseItem options chooseFunction
        

-- main loops
infoLoop :: String -> IO ()
infoLoop logText = do
    let pokeNames = getPokemon logText
    
    putStrLn "Enter the name of a Pokemon:"
    
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
                   ("TUTORS",    getTutorMoves   logStr pokemon)]
    input <- getLine
    
    let isQuitting = elem (upperCase input) ["Q","C", "B", "QUIT","CLOSE","BACK"]
    unless isQuitting (do -- requests for data loop back after displaying
        let match   = find  (isPrefixOf (upperCase input) . fst) options -- get the first pair where the input prefixes the string 
            result  = liftM (show . snd) match -- representation of the data (or Nothing on an invalid match)
        maybe (putStrLn "Invalid Command") putStrLn result
        pokeLoop logStr pokemon)
    
    infoLoop logStr
    


    