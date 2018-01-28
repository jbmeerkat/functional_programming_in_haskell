sum'n'count :: Integer -> (Integer, Integer)
sum'n'count 0 = (0, 1)
sum'n'count n = sum'n'countIter (abs n) 0 0

sum'n'countIter :: Integer -> Integer -> Integer -> (Integer, Integer)
sum'n'countIter 0 sum count = (sum, count)
sum'n'countIter n sum count =
    let digit = rem n 10
        new_n = quot n 10
        new_sum = sum + digit
        new_count = count + 1
    in sum'n'countIter new_n new_sum new_count
