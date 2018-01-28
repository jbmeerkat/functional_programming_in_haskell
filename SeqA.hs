seqA :: Integer -> Integer
seqA 0 = 1
seqA 1 = 2
seqA 2 = 3
seqA n = seqAIter 1 2 3 (n - 3)

seqAIter :: Integer -> Integer -> Integer -> Integer -> Integer
seqAIter a b c 0 = c + b - 2 * a
seqAIter a b c n = seqAIter b c (c + b - 2 * a) (n - 1)
