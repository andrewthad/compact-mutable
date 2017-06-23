{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Strict #-}

{-# OPTIONS_GHC -O2 #-}

module Data.Primitive.Compact
  ( CompactMutableArray
  , CompactPrimRef
  , Token
  , withToken
  , newCompactArray
  , newCompactPrimRef
  , readCompactArray
  , writeCompactArray
  , copyCompactMutableArray
  , getSizeOfCompact
  -- * Unsafe things
  , compactAddGeneral
  , unsafeInsertCompactArray
  , printCompactArrayAddrs
  ) where

import GHC.Int
import GHC.Prim
import GHC.Compact
import Control.Monad
import Control.Monad.Primitive
import Data.Primitive.SmallArray
import Data.Primitive.Array
import Data.Primitive.PrimArray
import Data.Primitive.Types
import Data.Primitive.PrimRef
import Unsafe.Coerce
import Debug.Trace
import GHC.Conc (pseq)
import GHC.Ptr (Ptr(..))
import GHC.Word (Word(..))

-- | This represents a mutable array in a compact region.
newtype CompactMutableArray s (a :: k -> *) (c :: k)
  = CompactMutableArray (MutablePrimArray s Addr) -- (Array (a c))
newtype CompactPrimRef s a c
  = CompactPrimRef (PrimRef s a)
data Token c = Token Compact#

-- withToken :: PrimMonad m => (forall c. Token c -> m x) -> m x
-- withToken f = compactNewGeneral >>= f

withToken :: PrimMonad m => (forall c. Token c -> m x) -> m x
withToken f = do
  !token <- compactNewGeneral
  !x <- f token
  !_ <- compactAddGeneral token (42 :: Int)
  return x

compactNewGeneral# :: Word# -> State# s -> (# State# s, Compact# #)
compactNewGeneral# = unsafeCoerce compactNew#

compactAddGeneral# :: Compact# -> a -> State# s -> (# State# s, a #)
compactAddGeneral# = unsafeCoerce compactAdd#

compactSizeGeneral# :: Compact# -> State# s -> (# State# s, Word# #)
compactSizeGeneral# = unsafeCoerce compactSize#


-- Does not typecheck
-- addByteArray :: Compact# -> State# RealWorld -> State# RealWorld
-- addByteArray c s1 = case newByteArray# 50# s1 of
--   (# s2, arr #) -> case compactAdd# c arr s2 of
--     (# s3, arr2 #) -> s3

compactAddGeneral :: PrimMonad m => Token c -> a -> m a
compactAddGeneral (Token compact#) !a = primitive $ \ !s ->
  compactAddGeneral# compact# a s

-- Notice that @c@ is universally quantified here.
compactNewGeneral :: PrimMonad m => m (Token c)
compactNewGeneral = primitive $ \ !s ->
  case compactNewGeneral# 31268## s of { (# !s1, !c #) ->
    (# s1, Token c #) }

getSizeOfCompact :: PrimMonad m => Token c -> m Word
getSizeOfCompact (Token compact#) = primitive $ \s0 ->
   case compactSizeGeneral# compact# s0 of (# s1, sz #) -> (# s1, W# sz #)

-- | Create a new mutable array.
newCompactPrimRef :: (PrimMonad m, Prim a)
  => Token c
  -> a -- ^ initial value
  -> m (CompactPrimRef (PrimState m) a c)
newCompactPrimRef !c !a = do
  !ref1 <- newPrimRef a
  !ref2 <- compactAddGeneral c ref1
  return (CompactPrimRef ref2)

-- | Create a new mutable array. Notice that we do not need a
--   default value.
newCompactArray :: (PrimMonad m)
  => Token c
  -> Int
  -> m (CompactMutableArray (PrimState m) a c)
newCompactArray !c !n = do
  -- it is unfortunate that we have to do this allocation twice,
  -- once on the normal heap and once on the compact heap.
  !marr1 <- newPrimArray n
  !marr2 <- compactAddGeneral c marr1
  return (CompactMutableArray marr2)

writeCompactArray :: PrimMonad m
  => CompactMutableArray (PrimState m) a c
  -> Int
  -> a c -- ^ please do not pick something that has been unboxed into 
         --   a data constructor, segfaults lie that way.
  -> m ()
writeCompactArray (CompactMutableArray !marr) !ix !val = do
  let !addr = unsafeToAddr val
  -- traceM (showAddr addr)
  writePrimArray marr ix addr
{-# INLINE writeCompactArray #-}

readCompactArray :: PrimMonad m
  => CompactMutableArray (PrimState m) a c
  -> Int 
  -> m (a c)
readCompactArray (CompactMutableArray !marr) !ix = do
  !addr <- readPrimArray marr ix
  return (unsafeFromAddr addr)
{-# INLINE readCompactArray #-}

-- | Insert an element in the array, shifting the values right 
--   of the index. The array size should be big enough for this
--   shift, this is not checked.
unsafeInsertCompactArray :: PrimMonad m
  => Int -- ^ Size of the original array
  -> Int -- ^ Index
  -> a c -- ^ Value
  -> CompactMutableArray (PrimState m) a c -- ^ Array to modify
  -> m ()
unsafeInsertCompactArray !sz !i !x (CompactMutableArray !marr) = do
  copyMutablePrimArray marr (i + 1) marr i (sz - i)
  writePrimArray marr i (unsafeToAddr x)
{-# INLINE unsafeInsertCompactArray #-}

copyCompactMutableArray
  :: PrimMonad m
  => CompactMutableArray (PrimState m) a c -- ^ destination
  -> Int -- ^ destination offset
  -> CompactMutableArray (PrimState m) a c -- ^ source
  -> Int -- ^ source offset
  -> Int -- ^ length
  -> m ()
copyCompactMutableArray (CompactMutableArray !dest) !doff (CompactMutableArray !src) !soff !len
  = copyMutablePrimArray dest doff src soff len
{-# INLINE copyCompactMutableArray #-}

unsafeToAddr :: a -> Addr
unsafeToAddr a = pseq a (Addr (unsafeCoerce# a :: Addr#))
{-# INLINE unsafeToAddr #-}

-- not joking when I say that this is about as unsafe as it gets.
unsafeFromAddr :: Addr -> a
unsafeFromAddr (Addr x) = unsafeCoerce# x
{-# INLINE unsafeFromAddr #-}

showAddr :: Addr -> String
showAddr (Addr a#) = show (Ptr a#)

printCompactArrayAddrs :: CompactMutableArray RealWorld a c -> IO ()
printCompactArrayAddrs (CompactMutableArray marr) = do
  n <- getSizeofMutablePrimArray marr
  forM_ [0..(n - 1)] $ \ix -> do
    addr <- readPrimArray marr ix
    putStrLn (showAddr addr)
  

