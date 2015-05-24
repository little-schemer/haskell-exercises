--
-- 1. トリボナッチ数列
--
triPattern :: Int -> Int
triPattern 0 = 0
triPattern 1 = 0
triPattern 2 = 1
triPattern n = triPattern (n - 1) + triPattern (n - 2) + triPattern (n - 3)

triGuard :: Int -> Int
triGuard n
  | n == 0 = 0
  | n == 1 = 0
  | n == 2 = 1
  | otherwise = triGuard (n - 1) + triGuard (n - 2) + triGuard (n - 3)

triCase :: Int -> Int
triCase n = case n of
  0 -> 0
  1 -> 0
  2 -> 1
  _ -> triCase (n - 1) + triCase (n - 2) + triCase (n - 3)


tribonacci :: [Int]
tribonacci = loop 0 0 1 where loop a b c = a : loop b c (a + b + c)


--
-- 2. タプル数
--
type Tuple = (Int, Int)

qadd :: Tuple -> Tuple -> Tuple
qadd (a, b) (c, d) = if b * d == 0
                     then error "second of tuple number is 0"
                     else (a * d + b * c, b * d)

qequal :: Tuple -> Tuple -> Bool
qequal (a, b) (c, d) = if b * d == 0
                       then error "second of tuple number is 0"
                       else a * d == b * c
