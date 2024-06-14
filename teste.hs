-- Max element in list
maxElement :: [Int] -> Int
maxElement [] = error "Empty list"
maxElement [x] = x
maxElement (x:xs) = max x (maxElement xs)

main :: IO ()
main = do
    let nums = [5,1,7,8,1,3]
    let result = maxElement nums
    putStrLn ("Result: " ++ show result)