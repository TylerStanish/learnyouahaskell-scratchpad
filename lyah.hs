lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER 7!!"
lucky x = "out of luck pal"

head' :: [a] -> a
head' (x:xs) = x
head' [] = error "can't call head on empty list dummy!"

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
  where (f:_) = firstname
        (l:_) = lastname


calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
  where bmi weight height = weight / height ^ 2

calcBmis' :: (RealFloat a) => [(a, a)] -> [a]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]


maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs)
  | x > maxTail = x
  | otherwise = maxTail
  where maxTail = maximum' xs


replicate' :: (Ord i, Num i) => i -> a -> [a]
replicate' n x
  | n <= 0 = []
  | otherwise = x:replicate' (n-1) x


take' :: (Integral i) => i -> [a] -> [a]
take' n _
  | n <= 0      = []
-- "Also notice that we use a guard, but without an otherwise part.
-- That means that if n turns out to be more than 0, the matching
-- will fall through to the next pattern"
take' _ []      = []
take' n (x:xs)  = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x:repeat' x

zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y):zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs) = a == x || elem' a xs

elem'' :: (Eq a) => a -> [a] -> Bool
elem'' _ [] = False
elem'' a (x:xs)
  | a == x    = True
  | otherwise = elem a xs

quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) = quicksort' [i | i <- xs, i < x] ++ [x] ++ quicksort' [i | i <- xs, i >= x]

quicksort'' :: (Ord a) => [a] -> [a]
quicksort'' [] = []
quicksort'' (x:xs) =
  let
    left = quicksort' [i | i <- xs, i <= x]
    right = quicksort' [i | i <- xs, i > x]
  in left ++ [x] ++ right


compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred a = compare 100 a

compareWithHundred' :: (Num a, Ord a) => a -> Ordering
compareWithHundred' = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum c = elem c ['A'..'Z']

applyTwice :: (a -> a) -> a -> a
applyTwice f a = f(f a)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f [] _ = []
zipWith' f _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- flip' :: (a -> b -> c) -> (b -> a -> c)
-- "-> is right associative by default" so we don't need parens
flip' :: (a -> b -> c) -> b -> a -> c
flip' f = g
  where g x y = f y x

flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f x y = f y x

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs


filter' :: (a -> Bool) -> [a] -> [a]
filter' f a = [i | i <- a, f i]

quicksortFilter :: (Ord a) => [a] -> [a]
quicksortFilter [] = []
quicksortFilter (x:xs) = 
  let smallerSorted = quicksortFilter $ filter (<=x) xs
      largerSorted = quicksortFilter $ filter (>x) xs
  in smallerSorted ++ [x] ++ largerSorted


largestDivisible :: (Integral a) => a -> a
largestDivisible a = head (filter p [100000..])
  where p x = (x `mod` 3829) == 0


sumSquaresUnder :: (Integral a) => a -> a
sumSquaresUnder maxNum = sum $ takeWhile (<maxNum) (map (^2) [1..])


chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain a
  | odd a = a : chain (a*3+1)
  | otherwise = a : chain (a `div` 2)


numLongChains :: (Integral a) => a -> Int
numLongChains a = length $ filter (>15) $ chain a
