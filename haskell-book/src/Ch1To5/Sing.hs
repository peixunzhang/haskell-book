module Ch1To5.Sing where

fstString :: [Char] -> [Char]
fstString x = x ++ "in the rain"

sndString :: [Char] -> [Char]
sndString x = x ++ " over the rainbow"

sing :: Ord a => a -> a -> [Char]
sing x y = if (x > y) then fstString "Singin" else sndString "somewhere"
