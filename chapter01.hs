type Point = (Int, Int)


--
-- 1. マンハッタン距離
--
manLen :: Point -> Point -> Int
manLen (a, b) (c, d) = abs (a - c) + abs (b - d)


--
-- 2. 整数座標のリスト
--
points :: Int -> [Point]
points n = [(x, y) | x <- nums, y <- nums] where nums = [negate n .. n]


--
-- 3. タクシー幾何学における円
--
manCircle :: Int -> [Point]
manCircle n = filter ((== 0) . (manLen (0, 0))) $ points n
