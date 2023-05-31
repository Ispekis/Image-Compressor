{-
-- EPITECH PROJECT, 2023
-- B-FUN-400-PAR-4-1-compressor-vincent.shao
-- File description:
-- Tools
-}

module Tools where

import Control.Exception (try, IOException)
import System.Exit
import System.IO
import Text.Read

exitWithMsg :: String -> ExitCode -> IO ()
exitWithMsg str code = hPutStrLn stderr str >> exitWith code

readInt :: [Char] -> Maybe Int
readInt str = readMaybe str :: Maybe Int

readFloat :: [Char] -> Maybe Float
readFloat str = readMaybe str :: Maybe Float


readingFile :: String -> IO (Maybe String)
readingFile path = do
    result <- try (readFile path) :: IO (Either IOException String)
    case result of
        Left _ -> return Nothing
        Right contents -> return (Just contents)

displayUsage :: IO ()
displayUsage = hPutStrLn stderr "USAGE ./imageCompressor -n N -l L -f F\n" >>
    hPutStrLn stderr "\tN\tnumber of colors in the final image" >>
    hPutStrLn stderr "\tL\tconvergence limit" >>
    hPutStrLn stderr"\tF\tpath to the file containing the colors of the pixels"

removeNthList :: Int -> [a] -> [a]
removeNthList _ [] = []
removeNthList 0 xs = xs
removeNthList n (_:xs) = removeNthList (n - 1) xs