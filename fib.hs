import Control.Exception (assert)

main :: IO ()
main = do
  check_result

check_result :: IO ()
check_result = do
  print $ assert (result == 9227465) result
  where result = (fib_naive 35)


fib_naive :: Int -> Int
fib_naive 1 = 1
fib_naive 2 = 1
fib_naive n = fib_naive (n-1) + fib_naive (n-2)
