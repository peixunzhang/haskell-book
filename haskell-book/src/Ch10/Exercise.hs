module Ch10.Exercise where
import Data.Time
data DatabaseItem = DbString String
                | DbNumber Integer
                | DbDate UTCTime
                deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase = 
    [DbDate (UTCTime 
        (fromGregorian 1911 5 1)
    (secondsToDiffTime 34123))
    , DbNumber 9001
    , DbString "Hello, world!"
    , DbDate (UTCTime
                (fromGregorian 1921 5 1)
                (secondsToDiffTime 34123))
    ]

-- Exercises: Database Processing
--1.
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr go []
    where
        go (DbDate time) acc = time:acc
        go _             acc = acc
--2.
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr f []
    where 
       f (DbNumber b ) a = b : a
       f  _ a = a
       
--3.
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate
--4.
sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

--5.
avgDb :: [DatabaseItem] -> Double
avgDb db = sumofvalues / numberofvalues
    where sumofvalues = fromIntegral . sumDb $ db
          numberofvalues = fromIntegral . length . filterDbNumber $ db

isDBNumber :: DatabaseItem -> Maybe Integer
isDBNumber (DbDate _) = Nothing
isDBNumber (DbNumber a) = Just a
isDBNumber (DbString _) = Nothing

--Scans exercise
--1.
fibs :: [Integer]
fibs = 1 : scanl (+) 1 fibs
fibsTwenty :: [Integer]
fibsTwenty = take 20 fibs
--2.
lessThanHunderd :: [Integer]
lessThanHunderd = takeWhile (< 100) fibs
--3.???
factorialF :: Integer -> [Integer]
factorialF n = scanl (*) 1 [2..n]
--1
--a
stops :: [Char]
stops = "pbtdkg"
vowels :: [Char]
vowels = "aeiou"
svs :: [(Char, Char, Char)]
svs = [(x, y, z) | x <- stops, y <- vowels, z <- stops]
--b
svsOnlyP :: [(Char, Char, Char)]
svsOnlyP = [(x, y, z) | x <- take 1 stops, y <- vowels, z <- stops]
--c


nouns :: [String]
nouns = ["chickens", "pencils", "soccer fields", "tables", "baseballs", "friends", "trees", "gardens", "Moms"]
verbs :: [String]
verbs = ["sing", "walk", "smell", "think", "fly", "jump", "run", "yell",  "wiggle", "play"]
nvn :: [(String, String, String)]
nvn = [(x, y, z) | x <- nouns, y <- verbs, z <- nouns]
--2
seekritFunc :: String -> Int
seekritFunc x = div (sum (map length (words x))) (length (words x))
--3
seekritFuncFra :: Fractional a => String -> a
seekritFuncFra x = (fromIntegral (sum (map length (words x)))) / (fromIntegral (length (words x)))
--fold exercise
--1
myOr :: [Bool] -> Bool
myOr = foldr (||) False 
--2
myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\x b -> f x || b) False
--3 use folding
myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr (\a b -> (a == x) || b) False
-- 3 use any
myElem' :: Eq a => a -> [a] -> Bool
myElem' a = any (== a)
--4
myReverse :: [a] -> [a]
myReverse = foldl (\acc x -> x:acc) []
--5
myMap :: (a -> b) -> [a] -> [b]
myMap f xs = foldr (\x xs -> (f x):xs) [] xs
--6
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f xs = foldr (\x xs -> if f x then x:xs else xs) [] xs
--7
squish :: [[a]] -> [a]
squish = foldr (++) []
--8
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squish . foldr (\x xs -> (f x):xs) []
--9
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id
-- --10
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = error "cannot take maximum of empty list"
myMaximumBy f (x:xs) = foldr cmp x xs
    where
        cmp a b = case f a b of
            GT -> b
            EQ -> a
            LT -> a
--11
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [] = error "cannot take maximum of empty list"
myMinimumBy f (x:xs) = foldr cmp x xs
    where
        cmp a b = case f a b of 
            GT -> a
            LT -> b
            EQ -> b