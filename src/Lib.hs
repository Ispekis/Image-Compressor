{-
-- EPITECH PROJECT, 2023
-- B-FUN-400-PAR-4-1-compressor-vincent.shao
-- File description:
-- Lib
-}

module Lib where

import Tools
import Data.Maybe
import Data.List
import Data.Ratio

import System.Random

data Conf = Conf {
    nbr :: Maybe Int,
    limit :: Maybe Float,
    path :: Maybe String
} deriving Show

type Short = Int
type Color = (Short, Short, Short)
type Point = (Int, Int)
type Pixel = (Point, Color)

defaultConf :: Conf
defaultConf = Conf {nbr = Nothing, limit = Nothing, path = Nothing}

getOpts :: Conf -> [String] -> Maybe Conf
getOpts (Conf n l p) []
    | (isNothing n) = Nothing
    | (isNothing l) = Nothing
    | (isNothing p) = Nothing
    | otherwise = Just (Conf n l p)
getOpts (Conf _ l p) ("-n":x:xs)
    | (isNothing (readInt x)) = Nothing
    | otherwise = getOpts (Conf (readInt x) l p) xs
getOpts (Conf n _ p) ("-l":x:xs)
    | (isNothing (readFloat x)) = Nothing
    | otherwise = getOpts (Conf n (readFloat x) p) xs
getOpts (Conf n l _) ("-f":x:xs) = getOpts (Conf n l (Just x)) xs
getOpts _ _ = Nothing

distance :: Color -> Color -> Float
distance (x1, y1, z1) (x2, y2, z2) =
    sqrt ((fromIntegral x2 - fromIntegral x1)^2 +
    (fromIntegral y2 - fromIntegral y1)^2 +
    (fromIntegral z2 - fromIntegral z1)^2)

convertVectorList :: [String] -> [(Float, Float, Float)]
convertVectorList = map parseVector
  where
    parseVector str =
      let values = map read $ words $ map
            (\c ->if c `elem` "()," then ' ' else c) str
      in (values !! 0, values !! 1, values !! 2)


mergeTuples :: [Color] -> [Short]
mergeTuples [] = []
mergeTuples ((x, y, z):xs) = x + y + z : (mergeTuples xs)

deduceDistance :: [Short] -> Short -> [Short]
deduceDistance [] _ = []
deduceDistance (x:xs) x1 = (abs (x1 - x)) : deduceDistance xs x1

getIndexSmallest :: [Short] -> Short -> Maybe Short
getIndexSmallest xs s = elemIndex s xs

computeEveryDistance :: [Color] -> Color -> [Float]
computeEveryDistance [] _ = []
computeEveryDistance (c:cs) v = (distance c v) : computeEveryDistance cs v

smallest :: [Float] -> Int -> Int -> Float -> Int
smallest [] _ index_save _ = index_save
smallest (x:xs) index index_save save
    | (x < save) = smallest xs (index + 1) index x
    | otherwise = smallest xs (index + 1) index_save save

closest :: [Color] -> Color -> Maybe Int
closest [] _ = Nothing
closest c v = Just (smallest (computeEveryDistance c v) 0 0 1000000000000)

displayPixels :: [(Point, Color)] -> IO ()
displayPixels [] = putStr ""
displayPixels ((p, c):xs) =
    putStr (show p)  >> putStr " " >> putStrLn (show c) >>
    displayPixels xs

displayCluster :: [(Color, [(Point, Color)])] -> IO ()
displayCluster [] = putStr ""
displayCluster (x:xs) =
    putStrLn "--" >>
    putStrLn (show (fst x)) >>
    putStrLn "-" >>
    displayPixels (snd x) >>
    displayCluster xs

strToPoint :: String -> Point
strToPoint str = read str :: Point

strToColor :: String -> Color
strToColor str = read str :: Color

convertInToData :: [String] -> [(Point, Color)]
convertInToData a = [((1,1), (1,1,1))]

stringToListTuple :: [String] -> [(Point, Color)]
stringToListTuple [] = []
stringToListTuple (x:y:xs) = (strToPoint x, strToColor y):stringToListTuple xs

randomNbr :: RandomGen g => g -> Int
randomNbr g = randomRs (0, 1) g!!1

getFirstCentroid :: RandomGen g => g -> Int -> Int
getFirstCentroid g max = randomRs (0, max - 1) g!!0

isDuplicate :: Eq a => [a] -> Bool
isDuplicate xs = length (nub xs) /= length xs

genRandomPoint :: RandomGen g => g -> Int -> Int -> [Int]
genRandomPoint _ 0 _ = []
genRandomPoint g dec max
    | isDuplicate (fst (randomR (0, max) g)
    :genRandomPoint (snd (randomR (0, max) g)) (dec - 1) max) =
        genRandomPoint ((snd (randomR (0, max) g))) dec max
    | otherwise = (fst (randomR (0, max) g):
    genRandomPoint (snd (randomR (0, max) g)) (dec - 1) max)

removeCentroids :: [(Point, Color)] -> [(Point, Color)] -> [(Point, Color)]
removeCentroids c l = c

setClosestWithIndex :: [Pixel] -> [Color] -> [(Int, (Point, Color))]
setClosestWithIndex [] _ = []
setClosestWithIndex (c:cs) cluster =
    (fromJust (closest cluster (snd c)), c):
    setClosestWithIndex cs cluster

getPixelsAtIndex :: [(Int, (Point, Color))] -> Int -> [(Point, Color)]
getPixelsAtIndex [] _ = []
getPixelsAtIndex (x:xs) i
    | (fst x) == i = snd x : getPixelsAtIndex xs i
    | otherwise = getPixelsAtIndex xs i

regroupClosests :: [Color] -> [(Int, (Point, Color))] -> Int -> [(Color, [(Point, Color)])]
regroupClosests [] _ _ = []
regroupClosests (x:xs) l i = (x, (getPixelsAtIndex l i)) :
    regroupClosests xs l (i + 1)

genEmptyCluster :: [Int] -> [(Point, Color)] -> [Color]
genEmptyCluster [] _ = []
genEmptyCluster (x:xs) l = snd (l!!x) : genEmptyCluster xs l

fstColor :: (a, b, c) -> a
fstColor (x,_,_) = x

sndColor :: (a, b, c) -> b
sndColor (_,y,_) = y

thdColor :: (a, b, c) -> c
thdColor (_,_,z) = z

getAverageColor :: [Color] -> Int -> Color
getAverageColor x d = (((div (foldr (+) 0 (map (fstColor) x)) d),
    (div (foldr (+) 0 (map (sndColor) x)) d),
    (div (foldr (+) 0 (map (thdColor) x)) d)))
-- getAverageColor x d = (round ((foldr (+) 0 (map (fstColor) x)) % d),
--     round ((foldr (+) 0 (map (sndColor) x)) % d),
--     round ((foldr (+) 0 (map (thdColor) x)) % d))

checkCanMove ::  Color -> Color -> Float -> Bool
checkCanMove old new limit
    | (distance old new) < limit = False
    | otherwise = True

atLeastOneMoved :: [(Color, [(Point, Color)])] -> [(Color, [(Point, Color)])] -> Float -> Bool
atLeastOneMoved [] [] _ = False
atLeastOneMoved (o:old) (c:cluster) limit
    | (checkCanMove (fst o) (fst c) limit) = True
    | otherwise = atLeastOneMoved old cluster limit

computeAverage :: [(Color, [(Point, Color)])] -> [(Color, [(Point, Color)])]
computeAverage [] = []
computeAverage (((c1,c2,c3),p):xs) =
    ((getAverageColor (map (snd) p) (length (map (snd) p))),[]):
    computeAverage xs

regroup :: [(Color, [(Point, Color)])] -> [Pixel] -> [(Color, [(Point, Color)])]
regroup new list = regroupClosests (map (fst) new)
    (setClosestWithIndex list (map (fst) new)) 0

runAlgo :: [(Color, [(Point, Color)])] -> [(Color, [(Point, Color)])] -> [Pixel] -> Float -> [(Color, [(Point, Color)])]
runAlgo old new list limit
    | (atLeastOneMoved old new limit) =
        runAlgo (regroup new list) (computeAverage old) list limit
    | otherwise = (regroup new list)

isOneClusterEmpty :: [(Color, [(Point, Color)])] -> Bool
isOneClusterEmpty [] = False
isOneClusterEmpty (c:cluster)
    | (length (snd c) == 0) = True
    | otherwise = isOneClusterEmpty cluster

runImageCompressor :: Conf -> Bool -> [Pixel] -> IO ()
runImageCompressor (Conf n l p) True list = do
    g <- initStdGen
    let cl = genEmptyCluster (genRandomPoint g (fromJust n)
            (length list - 1)) list
    if (isOneClusterEmpty (regroupClosests cl (setClosestWithIndex list cl) 0))
        then runImageCompressor (Conf n l p) True list
    else
        displayCluster (runAlgo (regroupClosests cl
        (setClosestWithIndex list cl) 0) (computeAverage (regroupClosests cl
        (setClosestWithIndex list cl) 0)) list (fromJust l))
runImageCompressor (Conf n l p) False _ = do
    g <- initStdGen
    content <- readingFile (fromJust p)
    let list = stringToListTuple (words (fromJust content))
    let cl = genEmptyCluster (genRandomPoint g (fromJust n)(length list-1))list
    if (isOneClusterEmpty (regroupClosests cl (setClosestWithIndex list cl) 0))
        then runImageCompressor (Conf n l p) True list
    else displayCluster (runAlgo (regroupClosests cl
        (setClosestWithIndex list cl) 0) (computeAverage(regroupClosests cl
        (setClosestWithIndex list cl) 0)) list (fromJust l))
