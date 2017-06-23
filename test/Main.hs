{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}

import Data.Primitive.Compact
import GHC.Compact
import GHC.Prim
import Data.Word
import Data.Primitive.Array
import Data.Primitive.PrimArray
import Data.Primitive.ByteArray
import Data.Primitive.PrimRef
import Data.Functor.Identity
import Data.Int
import Control.Monad

main :: IO ()
main = do
  putStrLn "Running compact-mutable tests"
  putStrLn "Trying normal compact functions as sanity check"
  _ <- newArray 13 (44 :: Int) >>= unsafeFreezeArray >>= compact
  _ <- newArray 11 (42 :: Int) >>= (\m -> freezeArray m 0 5) >>= compact
  nums <- newPrimArray 15 :: IO (MutablePrimArray RealWorld Int64)
  writePrimArray nums 0 58
  nums2 <- fmap getCompact $ compact nums
  nums2Alias <- fmap getCompact $ compact nums2
  writePrimArray nums2Alias 0 57
  originalVal <- readPrimArray nums 0
  copyVal <- readPrimArray nums2 0
  aliasVal <- readPrimArray nums2Alias 0
  when (originalVal /= 58) $ fail "original value wrong"
  when (copyVal /= 58) $ fail "copy value wrong"
  when (aliasVal /= 57) $ fail "alias value wrong"
  withToken $ \token -> do
    -- putStrLn "creating array"
    -- _ <- newCompactArray token 5
    -- putStrLn "creating mutable array"
    -- _ <- newCompactArray token 5
    -- putStrLn "creating array of arrays"
    -- c1 <- newCompactArray token 12
    -- c2 <- newCompactArray token 5
    -- writeCompactArray c1 0 (Yes c2)
    -- unsafeInsertCompactArray 4 2 (Yes c2) c1
    -- writeCompactArray c1 1 No
    -- x <- readCompactArray c1 1
    -- case x of
    --   No -> return ()
    --   Yes _ -> fail "did not get expected value"
    -- copyCompactMutableArray c1 0 c1 4 3
    -- c3 <- newCompactArray token 16
    -- _ <- compactAddGeneral token (Identity c1)
    -- _ <- compactAddGeneral token (Identity c3)
    -- putStrLn "creating PrimRef"
    p1 <- Ref <$> newPrimRef (12 :: Word16)
    p2 <- compactAddGeneral token p1
    -- p9 <- compactAddGeneral token (Thing (12 :: Word32))
    -- -- !p3 <- compactAddGeneral token p2
    -- _ <- newCompactArray token 3
    putStrLn "attempting large loop"
    arr <- newCompactArray token 10000000
    let go !n = if n < 10000000
          then do
            !p3 <- compactAddGeneral token (Thing n)
            writeCompactArray arr n p3
            go (n + 1)
          else return ()
    go 0
    -- printCompactArrayAddrs arr
    let goRead !n = if n < 10000000
          then do
            Thing val <- readCompactArray arr n
            -- val <- readPrimRef r
            if val == n
              then return ()
              else fail "found value not equal to n"
            goRead (n + 1)
          else return ()
    goRead 0
    putStrLn "finished large loop"
    putStrLn "aliasing behavior"
    a1 <- newCompactArray token 10
    writeCompactArray a1 0 (Thing (79 :: Int))
    a2 <- compactAddGeneral token a1
    a3 <- compactAddGeneral token a2
    writeCompactArray a3 0 (Thing (74 :: Int))
    Thing n <- readCompactArray a1 0
    when (n /= 74) $ fail "wrong value of n"
    putStrLn "finished aliasing behavior"
    return ()

-- Note: making types like this to put in a compact array is not
-- typically safe. Do not do it unless you understand how the compact
-- heap works.
data Thing a c = Thing !a
data MaybeArray c = No | Yes (CompactMutableArray RealWorld MaybeArray c)
data Ref c = Ref !(PrimRef RealWorld Word16)

