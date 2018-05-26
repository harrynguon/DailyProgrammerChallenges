import Data.Char(isUpper, isLower, toLower)

type Map a b = [(a, b)]

readInput :: String -> IO()
readInput input = print $ process input []

-- Update the count for each letter in the String
-- Once all letters in the String have been updated, sort the map 
process :: String -> (Map Char Int) -> (Map Char Int)
process [] map | isInDescOrder map = map
               | otherwise         = sort map []
process (x:xs) map | isLower x = process xs $ incrementCount x map []
                   | otherwise = process xs $ decrementCount (toLower x) map []

-- Increment the letter's count by 1. If it doesn't exist then initialise it
-- with a value of 1
incrementCount :: Char -> (Map Char Int) -> (Map Char Int) -> (Map Char Int)
incrementCount c [] seen = (c,1):seen
incrementCount c ((k,v):m) seen | c == k    = ((k,(v+1)):m) ++ seen
                                | otherwise = incrementCount c m ((k,v):seen)

-- Decrement the letter's count by 1. If it doesn't exist then create it with
-- a count of -1 
decrementCount :: Char -> (Map Char Int) -> (Map Char Int) -> (Map Char Int)
decrementCount c [] seen = (c,(-1)):seen
decrementCount c ((k,v):m) seen | c == k    = ((k,(v-1)):m) ++ seen
                                | otherwise = decrementCount c m ((k,v):seen)

-- Sort the map into descending order based on the count. This works by
-- recursively prepending the smallest element of the map, meaning the latest-
-- most prepended element is the largest (located at the front)
sort :: (Map Char Int) -> (Map Char Int) -> (Map Char Int)
sort [] sortedMap = sortedMap
sort map@((k,v):m) sortedMap = sort newMap (smallest:sortedMap)
                              where smallest = findSmallestElement map (k,v)
                                    newMap   = removeElement map smallestElement

-- Find the smallest element in the map by traversing through the map while
-- keeping track of the current smallest element found
findSmallestElement :: (Map Char Int) -> (Char, Int) -> (Char, Int)
findSmallestElement [] crtLowest = crtLowest
findSmallestElement ((k,v):m) crtLowest@(k2,v2)
                                    | v < v2 = findSmallest m (k,v)
                                    | otherwise = findSmallest m crtLowest

-- Remove the element in the map by filtering it out and then returning the
-- result
removeElement :: (Map Char Int) -> (Char, Int) -> (Map Char Int)
removeElement map (k,v) = filter (\(a,b) -> a /= k) map

-- Check if the map is descending order: first map it to a list of ints for
-- easier element access
isInDescOrder :: (Map Char Int) -> Bool
isInDescOrder oMap = checkOrder $ map(\(a,b) -> b) oMap

-- Then recursively compare the head with the next-most element throughout the
-- whole list
checkOrder :: [Int] -> Bool
checkOrder [] = True
checkOrder (x:y:xs) | length xs == 0 = True
                    | x == y         = checkOrder xs
                    | otherwise      = False