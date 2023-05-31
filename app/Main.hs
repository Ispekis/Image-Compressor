{-
-- EPITECH PROJECT, 2023
-- B-FUN-400-PAR-4-1-compressor-vincent.shao
-- File description:
-- Main
-}

module Main (main) where

import Lib
import System.Environment
import System.Exit
import Lib
import Tools

checkReading :: Conf -> IO Bool
checkReading (Conf n l (Just f)) = do
        res <- readingFile f
        case res of
            Nothing -> return False
            Just c -> return True

main :: IO ()
main = do
    args <- getArgs
    let result = getOpts defaultConf args
    case result of
        Just c -> do
                fileExist <- checkReading c
                case fileExist of
                    False -> displayUsage >> exitWith (ExitFailure 84)
                    True -> runImageCompressor c False []
        Nothing -> displayUsage >> exitWith (ExitFailure 84)
