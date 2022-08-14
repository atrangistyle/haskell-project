module Lib 
    (joe  
    ) where
        
import System.IO ( hClose, hGetContents, openFile, IOMode(ReadMode) )  

joe :: IO ()
joe = do  
    putStrLn "What's your file name?" 
    filename <- getLine  
    handle <- openFile filename ReadMode
    contents <- hGetContents handle
    print contents
    hClose handle   


-- someFunc :: IO ()
-- someFunc = putStrLn "someFunc"


