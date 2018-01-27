dist :: (Double, Double) -> (Double, Double) -> Double
dist p1 p2 = sqrt $ dx ^ 2 + dy ^ 2 where
  dx = fst p2 - fst p1
  dy = snd p2 - snd p1
