--Session 4 -1 Warm Up (Drilling on IO)

prog1 :: IO ()
prog1 = do
  m <- getLine :: IO Int
  n <- getLine :: IO Int
  replicateM_ m (print n)

prog2 :: IO ()
prog2 = do
  s <- getLine
  case s of
    "" -> return
    _ -> do
      print (reverse x)
      prog2

index :: [IO a] -> IO Int -> IO a
index ms = join . liftM (ms !!)
