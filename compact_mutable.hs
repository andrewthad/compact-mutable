{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MagicHash #-}

import Data.Compact
import Data.Primitive
import Data.IORef
import Control.Monad.ST (RealWorld)
import Unsafe.Coerce
import GHC.Prim (unsafeCoerce#)

main :: IO ()
main = do
  withCompactRef $ \c -> do
    carr <- newCompactArray c 1000
    let arr = getCompactValue carr
    writeCompactArray arr 0 =<< newCompactValue c (12 :: Int)
    num <- readCompactArray arr 0
    putStrLn (show (getCompactValue num))
  putStrLn "done"

-- | This represents a mutable array on a compact region.
newtype CompactArray c a = CompactArray (MutableArray RealWorld a)
newtype CompactValue c a = CompactValue a
newtype CompactRef c = CompactRef (Compact Token)
data Token = Token

withCompactRef :: (forall c. CompactRef c -> IO a) -> IO a
withCompactRef f = do
  c <- compact Token
  f (CompactRef c)

newCompactArray :: 
     CompactRef c
  -> Int
  -> IO (CompactValue c (CompactArray c a))
newCompactArray (CompactRef c) n = do
  let !defVal = unsafeFromToken (getCompact c)
  marr <- newArray n defVal
  Array uarr <- freezeArray marr 0 n
  let !arr1 = MutableArray (unsafeCoerce# uarr)
  !carr2 <- compactAdd c arr1
  let !arr2 = getCompact carr2
  return (CompactValue (CompactArray arr2))

newCompactValue :: CompactRef c -> a -> IO (CompactValue c a)
newCompactValue (CompactRef c) a = do
  !cb <- compactAdd c a
  let !b = getCompact cb
  return (CompactValue b)

writeCompactArray :: CompactArray c a -> Int -> CompactValue c a -> IO ()
writeCompactArray (CompactArray arr) ix (CompactValue !val) =
  writeArray arr ix val

readCompactArray :: CompactArray c a -> Int -> IO (CompactValue c a)
readCompactArray (CompactArray arr) ix = do
  !v <- readArray arr ix
  return (CompactValue v)

getCompactValue :: CompactValue c a -> a
getCompactValue (CompactValue a) = a

unsafeFromToken :: Token -> a
unsafeFromToken = unsafeCoerce

