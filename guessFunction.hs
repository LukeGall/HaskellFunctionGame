check :: (Int -> Int -> Int) -> Int -> Int-> [(Bool, Int, Int, Int)]
check fun input1 input2 = [((fun x y) == 0, fun x y, x, y) | x <- [input1..(input1+2)], y <- [input2..(input2+2)]]


turn :: (Int -> Int -> Int) -> Int -> Bool -> IO()
turn fun n correct =
    do if n==0
        then putStrLn "You lose"
        else if correct
            then putStrLn "You Win!"
            else mkguess fun n

mkguess :: (Int -> Int -> Int) -> Int -> IO()
mkguess fun n =
  do putStrLn ("  " ++ take n (repeat '*'))
     putStr "  Enter your first guess: "
     q <- getLine
     let fstI = read q :: Int
     putStr "  Enter your second guess: "
     q2 <- getLine
     let scdI = read q2 :: Int
     let lst = check fun fstI scdI
     let correct = or [b | (b,x,y,z) <- lst]
     putStrLn (foldr (++) [] ["'fun' "++show x++" "++ show y++" = "++show o++"\n" | (b,o,x,y) <- lst])
     let n' = if correct then n else n-1
     turn fun n' correct

guessFun :: (Int -> Int -> Int) -> Int -> IO()
guessFun fun guesses = turn fun guesses False