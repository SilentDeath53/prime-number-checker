divisibleByAny : Integer -> Integer -> Bool
divisibleByAny n i = i < n && n `mod` i == 0 || divisibleByAny n (i + 1)

isPrime : Integer -> Bool
isPrime n = n > 1 && not (divisibleByAny n 2)

main : IO ()
main = do
  putStrLn "Enter a number to check if it's prime:"
  numStr <- getLine
  let num = cast numStr
  case num of
    Nothing => putStrLn "Invalid input. Please enter a valid number."
    Just n => do
      let result = if isPrime n then "is prime." else "is not prime."
      putStrLn $ show n ++ " " ++ result
