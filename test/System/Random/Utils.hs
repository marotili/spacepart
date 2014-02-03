{-# LANGUAGE ScopedTypeVariables #-}
module System.Random.Utils where
import System.Random.Mersenne

randomElement a gen = do
    v :: Double <- random gen
    let i :: Int = floor $ v * (fromIntegral $ length a)
    return $ a !! i

randomRange low high gen = do
    v <- random gen
    return $ low * (1 - v) + high * v
