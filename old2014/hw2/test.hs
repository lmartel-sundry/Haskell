module Test where

assert :: (Show a, Eq a) => a -> a -> IO ()
assert got expected
    | got /= expected   = putStrLn $ "TEST FAILED: EXPECTED " ++ show expected ++ ", GOT " ++ show got
    | otherwise         = putStrLn $ "Test passed. Got " ++ show got

