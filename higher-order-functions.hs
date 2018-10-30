divideByTen :: (Floating a) => a -> a  
divideByTen = (/10)  

isUpperAlphanum :: Char -> Bool  
isUpperAlphanum = (`elem` ['A'..'Z'])  

applyTwice :: (a -> a) -> a -> a  
applyTwice f x = f (f x)  


zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]  
zipWith' _ [] _ = []  
zipWith' _ _ [] = []  
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys  


flip' :: (a -> b -> c) -> (b -> a -> c)  
flip' f = g  
    where g x y = f y x  

# or more simple:
flip' :: (a -> b -> c) -> b -> a -> c  
flip' f y x = f x y


map :: (a -> b) -> [a] -> [b]  
map _ [] = []  
map f (x:xs) = f x : map f xs  


filter :: (a -> Bool) -> [a] -> [a]  
filter _ [] = []  
filter p (x:xs)   
    | p x       = x : filter p xs  
    | otherwise = filter p xs    


quicksort :: (Ord a) => [a] -> [a]    
quicksort [] = []    
quicksort (x:xs) =     
    let smallerSorted = quicksort (filter (<=x) xs)  
        biggerSorted = quicksort (filter (>x) xs)   
    in  smallerSorted ++ [x] ++ biggerSorted


largestDivisible :: (Integral a) => a  
largestDivisible = head (filter p [100000,99999..])  
    where p x = x `mod` 3829 == 0  


# find the sum of all odd squares that are smaller than 10,000
# Solution 1
sum (takeWhile (<10000) (filter odd (map (^2) [1..])))  

# Solution 2
sum (takeWhile (<10000) [n^2 | n <- [1..], odd (n^2)])  


# Collatz sequences: 
# We take a natural number. 
# If that number is even, we divide it by two. 
# If it's odd, we multiply it by 3 and then add 1 to that. 
# We take the resulting number and apply the same thing to it, which produces a new number and so on.
chain :: (Integral a) => a -> [a]  
chain 1 = [1]  
chain n  
    | even n =  n:chain (n `div` 2)  
    | odd n  =  n:chain (n*3 + 1)  

# For all starting numbers between 1 and 100, how many chains have a length greater than 15?
numLongChains :: Int  
numLongChains = length (filter isLong (map chain [1..100]))  
    where isLong xs = length xs > 15  



