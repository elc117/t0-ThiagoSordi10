main = do
  putStrLn "Inicio"

isBin :: String -> Bool
isBin "" = False
isBin (x:xs) = if elem x "10"
  then if xs == ""
    then True
    else isBin xs 
  else False

isBin' :: String -> Bool 
isBin' s = all (\c -> elem c "10") s

auxBin2Dec :: [Int] -> Int -> Int
auxBin2Dec bits n = if length bits == 0
  then 0
  else ((head bits)*2^n) + auxBin2Dec (tail bits) (n-1)

bin2dec :: [Int] -> Int
bin2dec [] = undefined
bin2dec bits = auxBin2Dec bits ((length bits)-1)

bin2dec' :: [Int] -> Int
bin2dec' bits =  sum (zipWith (\x y -> x*2^y) [x | x <- bits] [y | y <- [(length bits)-1,(length bits)-2..0]])

dec2bin :: Int -> [Int]
dec2bin 0 = []
dec2bin n = dec2bin (div n 2) ++ [mod n 2]

isHex :: String -> Bool
isHex "" = False
isHex (x:xs) = if elem x "0123456789ABCDEF"
  then if xs == ""
    then True
    else isHex xs
  else False