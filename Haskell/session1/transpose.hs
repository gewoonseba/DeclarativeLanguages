-- Session 1 - 4 Transpose

transpose :: [[a]] -> [[a]]
transpose ([]:_) = [[]]
transpose xs = map head xs : transpose (map tail xs)

--map head xs produceert lijst met alle eerste elementen, herhalen voor de tail
