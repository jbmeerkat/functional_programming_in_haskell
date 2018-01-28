fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci (-1) = 1
fibonacci n
  | n < 0 = (-1) ^ (abs n + 1) * fibonacci(abs n)
  | n > 0 = fibonacciIter 0 1 (n - 2)

fibonacciIter previous current 0 = previous + current
fibonacciIter previous current n =
  fibonacciIter current (previous + current) (n - 1)
