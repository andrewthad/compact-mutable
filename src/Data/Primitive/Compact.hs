{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Primitive.Compact
  ( CompactMutableArray
  , CompactValue
  , Token
  , newCompactArray
  , newCompactValue
    -- * Read and write
  , readCompactArray
  , writeCompactArray
  ) where

import GHC.Int
import GHC.Prim
import GHC.Compact
import Control.Monad.Primitive
import Data.Primitive.SmallArray
import Unsafe.Coerce

-- | This represents a mutable array in a compact region.
newtype CompactMutableArray c s a
  = CompactMutableArray (SmallMutableArray s a)
newtype CompactValue c a
  = CompactValue a
data Token c = Token Compact#
data Placeholder = Placeholder

compactAddGeneral# :: Compact# -> a -> State# s -> (# State# s, a #)
compactAddGeneral# = unsafeCoerce compactAdd#

compactAddGeneral :: PrimMonad m => Token c -> a -> m a
compactAddGeneral (Token compact#) a = primitive $ \s ->
  case compactAddGeneral# compact# a s of { (# s1, pk #) ->
    (# s1, pk #) }

-- | Create a new mutable array.
newCompactArray :: (PrimMonad m)
  => Token c
  -> Int
  -> m (CompactValue c (CompactMutableArray c (PrimState m) a))
newCompactArray c n = do
  let !defVal = unsafeFromPlaceholder Placeholder
  marr <- newSmallArray n defVal
  stdHeapArr <- freezeSmallArray marr 0 n
  SmallArray cmptHeapArr <- compactAddGeneral c stdHeapArr
  let !cmptHeapMutArr = unsafeCoerce# cmptHeapArr
  return (CompactValue (CompactMutableArray (SmallMutableArray cmptHeapMutArr)))

newCompactValue :: PrimMonad m
  => Token c
  -> a
  -> m (CompactValue c a)
newCompactValue t a = do
  !v <- compactAddGeneral t a
  return (CompactValue v)

writeCompactArray :: (PrimMonad m, m ~ IO)
  => CompactMutableArray c (PrimState m) a 
  -> Int
  -> CompactValue c a
  -> m ()
writeCompactArray (CompactMutableArray arr) ix (CompactValue !val) =
  writeSmallArray arr ix val

readCompactArray :: (PrimMonad m, m ~ IO)
  => CompactMutableArray c (PrimState m) a
  -> Int 
  -> IO (CompactValue c a)
readCompactArray (CompactMutableArray arr) ix = do
  !v <- readSmallArray arr ix
  return (CompactValue v)

getCompactValue :: CompactValue c a -> a
getCompactValue (CompactValue a) = a

unsafeFromPlaceholder :: Placeholder -> a
unsafeFromPlaceholder = unsafeCoerce
