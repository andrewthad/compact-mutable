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
newtype Token c
  = Token (Compact Placeholder)
data Placeholder = Placeholder

-- | Create a new mutable array.
newCompactArray :: (PrimMonad m, m ~ IO)
  => Token c
  -> Int
  -> m (CompactValue c (CompactMutableArray c (PrimState m) a))
newCompactArray (Token c) n = do
  let !defVal = unsafeFromPlaceholder (getCompact c)
  marr <- newSmallArray n defVal
  stdHeapArr <- freezeSmallArray marr 0 n
  cmpt <- compactAdd c stdHeapArr
  let SmallArray cmptHeapArr = getCompact cmpt
      !cmptHeapMutArr = unsafeCoerce# cmptHeapArr
  return (CompactValue (CompactMutableArray (SmallMutableArray cmptHeapMutArr)))

newCompactValue :: (PrimMonad m, m ~ IO)
  => Token c
  -> a
  -> m (CompactValue c a)
newCompactValue (Token c) a = do
  !cb <- compactAdd c a
  let !b = getCompact cb
  return (CompactValue b)

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
