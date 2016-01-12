--Session 1 - 3 Unpair

unpair :: [(a,b)] -> ([a],[b])
unpair [] = ([],[]) --niet nodig
unpair xs = (map fst xs, map snd xs)
