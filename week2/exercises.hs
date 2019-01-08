vecLen :: [Double] -> Double
vecLen arr = sqrt $ sum $ map (\x -> x^2) arr

matches :: [Int] -> [Int] -> Int
matches arr1 arr2 = do
  let xs = zipWith (\x  y -> x==y) arr1 arr2
  foldl (\x y -> if (y==True) then (x + 1) else x) 0 xs

numRepeats :: [Int] -> Int
numRepeats x = matches x (tail x)

isIn :: Int -> [Int] -> Bool
isIn x xs = foldr ((||) . (== x)) False xs

removeAtIndex :: Int -> [a] -> [a]
removeAtIndex n xs = take (n -1) xs ++ drop n xs

unique :: [Int] -> Bool
unique xs = do
  let indexed = zip xs [0..]
  not . or $ map (\(a,b) -> isIn a  $ removeAtIndex b xs) indexed

doToAll :: (Int -> Int) -> [Int] -> [Int]
doToAll f xs = zipWith (\x fn -> (fn x)) xs (repeat f)

poly :: Num a => [a] -> a -> a
poly coeffs x = do
  let coefficientRange = [(length coeffs), ((length coeffs) - 1).. 0]
  sum $ zipWith (\num coeff -> if coeff == 1 then num else num * (x ^ (coeff -1))) coeffs coefficientRange

headOverHeels :: [String] -> [String]
headOverHeels xs
  | xs == [] = []
  | otherwise = (tail xs) ++ [head xs]
