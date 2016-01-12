doubleMe x = x + x

doubleUs x y = x * 2 + y * 2

doubleSmallNumber x = if x > 100
                      then x
                      else doubleMe x

--Pattern matching
tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell(x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell(x:y:_) = "The list is long. The first two elements are :" ++ show x ++ " and " ++ show y

--as-Pattern
firstLetter :: String -> String
firstLetter "" = "Empty String, whoops!"
firstLetter all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

--guards
bmiTell' :: Double -> String
bmiTell' bmi
    | bmi <= 18.5 = "You're underweight, you emo, you!"
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you'e ugly!"
    | bmi <= 30.0 = "You're fat! Lose some weight fatty!"
    | otherwise = "You're a whale, congratulations!"

bmiTell :: Double -> Double -> String
bmiTell weight height
    | weight / height ^ 2 <= 18.5 = "You're underweight"
    | weight / height ^ 2 <= 25.0 = "Normal"
    | weight / height ^ 2 <= 30.0 = "Fat"
    | otherwise = "Whale"

--Using where
bmiTell'' :: Double -> Double -> String
bmiTell'' weight height
    | bmi  <= 18.5 = "You're underweight"
    | bmi  <= 25.0 = "Normal"
    | bmi <= 30.0 = "Fat"
    | otherwise = "Whale"
    where bmi = weight / height ^ 2
