{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Strict #-}

{-# OPTIONS_GHC -O2 #-}

module Data.Primitive.Compact
  ( CompactMutableArray(..)
  , CompactMutablePrimArray(..) -- should not actually export this
  , CompactMutableByteArray(..)
  , ContractedMutableArray(..)
  , CompactPrimRef
  , Token
  , Contractible(..)
  , Heap(..)
  , withToken
  , newCompactArray
  , newContractedArray
  , newCompactArrayCopier
  , newCompactPrimRef
  , newCompactPrimArray
  , newCompactPrimArrayCopier
  , readCompactArray
  , writeCompactArray
  , copyCompactMutableArray
  , copyContractedMutableArray
  , getSizeOfCompact
  , readContractedArray
  , writeContractedArray
  -- * Unsafe things
  , compactAddGeneral
  , unsafeInsertCompactArray
  , unsafeInsertContractedArray
  , printCompactArrayAddrs
  ) where

import GHC.Int
import GHC.Prim
import GHC.Compact
import Control.Monad
import Control.Monad.Primitive
import Data.Primitive
import Data.Primitive.SmallArray
import Data.Primitive.Array
import Data.Primitive.PrimArray
import Data.Primitive.Types
import Data.Primitive.PrimRef
import Data.Primitive.ByteArray
import Data.Primitive.UnliftedArray
import Data.Proxy
import Unsafe.Coerce
import Debug.Trace
import GHC.Conc (pseq)
import GHC.Ptr (Ptr(..))
import GHC.Word (Word(..))
import GHC.Types

-- | This represents a mutable array in a compact region.
newtype CompactMutableArray s (a :: Heap -> *) (c :: Heap)
  = CompactMutableArray (MutablePrimArray s Addr) -- (Array (a c))
newtype CompactPrimRef s a c
  = CompactPrimRef (PrimRef s a)
newtype CompactMutablePrimArray s a c
  = CompactMutablePrimArray (MutablePrimArray s a)
newtype CompactMutableByteArray s c
  = CompactMutableByteArray (MutableByteArray s)

-- | A contracted array is like a prim array, except that
--   it talks about data structures who have data on a
--   compact heap.
data ContractedMutableArray (a :: * -> Heap -> *) s (c :: Heap)
  = ContractedMutableArray
    (MutableByteArray# s)
    (MutableArrayArray# s)
data Token (c :: Heap) = Token Compact#
data Heap

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

newCompactPrimArray :: (PrimMonad m, Prim a)
  => Token c
  -> Int -- ^ Length
  -> m (CompactMutablePrimArray (PrimState m) a c)
newCompactPrimArray !c !n = do
  !ref1 <- newPrimArray n
  !ref2 <- compactAddGeneral c ref1
  return (CompactMutablePrimArray ref2)

newCompactPrimArrayCopier :: (PrimMonad m, Prim a)
  => Token c
  -> Int -- ^ Length
  -> m (m (CompactMutablePrimArray (PrimState m) a c))
newCompactPrimArrayCopier !c !n = do
  !marr1 <- newPrimArray n
  return $ do
    !marr2 <- compactAddGeneral c marr1
    return (CompactMutablePrimArray marr2)

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

newContractedArray :: forall m a c. (PrimMonad m, Contractible a)
  => Token c
  -> Int
  -> m (ContractedMutableArray a (PrimState m) c)
newContractedArray !c !n = do
  -- it is unfortunate that we have to do this allocation twice,
  -- once on the normal heap and once on the compact heap.
  MutableByteArray ba <- compactAddGeneral c =<< newByteArray (n * I# (unsafeContractedByteCount# (proxy# :: Proxy# a)))
  UnliftedArray aa <- compactAddGeneral c =<< unsafeFreezeUnliftedArray =<< newUnliftedArray (n * I# (unsafeContractedUnliftedPtrCount# (proxy# :: Proxy# a))) (MutableByteArray ba)
  return (ContractedMutableArray ba (unsafeCoerce# aa))

newCompactArrayCopier :: PrimMonad m
  => Token c
  -> Int
  -> m (m (CompactMutableArray (PrimState m) a c))
newCompactArrayCopier !c !n = do
  !marr1 <- newPrimArray n
  return $ do
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

unsafeInsertContractedArray :: forall m a c. (PrimMonad m, Contractible a)
  => Int -- ^ Size of the original array
  -> Int -- ^ Index
  -> a (PrimState m) c -- ^ Value
  -> ContractedMutableArray a (PrimState m) c -- ^ Array to modify
  -> m ()
unsafeInsertContractedArray !sz !i !x !carr = do
  copyContractedMutableArray carr (i + 1) carr i (sz - i)
  writeContractedArray carr i x
{-# INLINE unsafeInsertContractedArray #-}

copyContractedMutableArray
  :: forall m a c. (PrimMonad m, Contractible a)
  => ContractedMutableArray a (PrimState m) c -- ^ destination
  -> Int -- ^ destination offset
  -> ContractedMutableArray a (PrimState m) c -- ^ source
  -> Int -- ^ source offset
  -> Int -- ^ length
  -> m ()
copyContractedMutableArray (ContractedMutableArray destB destA) (I# destOff) (ContractedMutableArray srcB srcA) (I# srcOff) (I# len)
  = primitive_ $ \s1 -> case copyMutableByteArray# srcB (srcOff *# (unsafeContractedByteCount# (proxy# :: Proxy# a))) destB (destOff *# (unsafeContractedByteCount# (proxy# :: Proxy# a))) (len *# (unsafeContractedByteCount# (proxy# :: Proxy# a))) s1 of
      s2 -> copyMutableArrayArray# srcA (srcOff *# (unsafeContractedUnliftedPtrCount# (proxy# :: Proxy# a))) destA (destOff *# (unsafeContractedUnliftedPtrCount# (proxy# :: Proxy# a))) (len *# (unsafeContractedUnliftedPtrCount# (proxy# :: Proxy# a))) s2
{-# INLINE copyContractedMutableArray #-}

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

writeContractedArray :: (PrimMonad m, Contractible a)
  => ContractedMutableArray a (PrimState m) c -> Int -> a (PrimState m) c -> m ()
writeContractedArray (ContractedMutableArray ba aa) (I# ix) r =
  primitive_ $ \s -> writeContractedArray# ba aa ix r s

readContractedArray :: (PrimMonad m, Contractible a)
  => ContractedMutableArray a (PrimState m) c -> Int -> m (a (PrimState m) c)
readContractedArray (ContractedMutableArray ba aa) (I# ix) =
  primitive $ \s -> readContractedArray# ba aa ix s

-- | Super dangerous typeclass. Be careful trying to
--   define instances.
class Contractible (a :: * -> Heap -> *) where
  unsafeContractedUnliftedPtrCount# :: Proxy# a -> Int#
  -- ^ Number of unlifted pointers
  unsafeContractedByteCount# :: Proxy# a -> Int#
  -- ^ size of serialization, in bytes
  writeContractedArray# :: MutableByteArray# s -> MutableArrayArray# s -> Int# -> a s c -> State# s -> State# s
  -- ^ remember that the int provided here gives an index in
  --   elements, not in bytes
  readContractedArray# :: MutableByteArray# s -> MutableArrayArray# s -> Int# -> State# s -> (# State# s, a s c #)
  -- ^ index is in elements, not bytes

newtype Heaped a s (c :: Heap) = Heaped a

instance Prim a => Contractible (Heaped a) where
  unsafeContractedUnliftedPtrCount# _ = 0#
  unsafeContractedByteCount# _ = sizeOf# (undefined :: a)
  writeContractedArray# arr _ ix (Heaped a) s = writeByteArray# arr ix a s
  readContractedArray# arr _ ix s = case readByteArray# arr ix s of
    (# s', a #) -> (# s', Heaped a #)

-- instance Contractible (CompactMutableArray m a) where
--   unsafeContractedUnliftedPtrCount _ = 1
--   unsafeContractedByteCount _ = 0
--   unsafeWriteContractedArray (ContractedMutableArray _ marr#) (I# ix) (CompactMutableArray (MutablePrimArray arr#)) =
--     primitive_ (\s -> writeMutableByteArrayArray# marr# ix arr# s)
--   unsafeReadContractedArray (ContractedMutableArray _ marr#) (I# ix) =
--     primitive (\s -> case readMutableByteArrayArray# marr# ix s of
--       (# s', arr# #) -> (# s', CompactMutableArray (MutablePrimArray arr# ) #)
--     )
  


