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
  , sizeOfContractedElement
  -- * Unsafe things
  , compactAddGeneral
  , unsafeInsertCompactArray
  , unsafeInsertContractedArray
  , printCompactArrayAddrs
  , unsafeUnliftedToAddr
  , unsafeUnliftedFromAddr
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
newtype ContractedMutableArray s (a :: Heap -> *) (c :: Heap)
  = ContractedMutableArray (MutableByteArray s)
data Token c = Token Compact#
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
  -> m (ContractedMutableArray (PrimState m) a c)
newContractedArray !c !n = do
  -- it is unfortunate that we have to do this allocation twice,
  -- once on the normal heap and once on the compact heap.
  !marr1 <- newByteArray (n * sizeOfContractedElement (Proxy :: Proxy a))
  !marr2 <- compactAddGeneral c marr1
  return (ContractedMutableArray marr2)

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
  -> a c -- ^ Value
  -> ContractedMutableArray (PrimState m) a c -- ^ Array to modify
  -> m ()
unsafeInsertContractedArray !sz !i !x !carr = do
  copyContractedMutableArray carr (i + 1) carr i (sz - i)
  writeContractedArray carr i x
{-# INLINE unsafeInsertContractedArray #-}

copyContractedMutableArray
  :: forall m a c. (PrimMonad m, Contractible a)
  => ContractedMutableArray (PrimState m) a c -- ^ destination
  -> Int -- ^ destination offset
  -> ContractedMutableArray (PrimState m) a c -- ^ source
  -> Int -- ^ source offset
  -> Int -- ^ length
  -> m ()
copyContractedMutableArray (ContractedMutableArray !dest) !doff (ContractedMutableArray !src) !soff !len
  = copyMutableByteArray dest (doff * sizeOfContractedElement (Proxy :: Proxy a)) src (soff * sizeOfContractedElement (Proxy :: Proxy a)) (len * sizeOfContractedElement (Proxy :: Proxy a))
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

-- make sure this is actually sound. I'm pretty sure that
-- unlifted data must already be fully evaluated because of
-- how its calling convention works.
unsafeUnliftedToAddr :: forall (a :: TYPE 'UnliftedRep). a -> Addr
unsafeUnliftedToAddr a = Addr (unsafeCoerce# a :: Addr#)
{-# INLINE unsafeUnliftedToAddr #-}

unsafeUnliftedFromAddr :: forall (a :: TYPE 'UnliftedRep). Addr -> a
unsafeUnliftedFromAddr (Addr x) = unsafeCoerce# x
{-# INLINE unsafeUnliftedFromAddr #-}

showAddr :: Addr -> String
showAddr (Addr a#) = show (Ptr a#)

printCompactArrayAddrs :: CompactMutableArray RealWorld a c -> IO ()
printCompactArrayAddrs (CompactMutableArray marr) = do
  n <- getSizeofMutablePrimArray marr
  forM_ [0..(n - 1)] $ \ix -> do
    addr <- readPrimArray marr ix
    putStrLn (showAddr addr)

sizeOfContractedElement :: Contractible a => Proxy a -> Int
sizeOfContractedElement = unsafeSizeOfContractedElement

writeContractedArray :: (PrimMonad m, Contractible a) => ContractedMutableArray (PrimState m) a c -> Int -> a c -> m ()
writeContractedArray = unsafeWriteContractedArray

readContractedArray :: (PrimMonad m, Contractible a) => ContractedMutableArray (PrimState m) a c -> Int -> m (a c)
readContractedArray = unsafeReadContractedArray

-- | Super dangerous typeclass. Be careful trying to
--   define instances.
class Contractible (a :: Heap -> *) where
  unsafeSizeOfContractedElement :: Proxy a -> Int
  -- ^ size of serialization, in bytes
  unsafeWriteContractedArray :: PrimMonad m => ContractedMutableArray (PrimState m) a c -> Int -> a c -> m ()
  -- ^ remember that the int provided here gives an index in
  --   elements, not in bytes
  unsafeReadContractedArray :: PrimMonad m => ContractedMutableArray (PrimState m) a c -> Int -> m (a c)
  -- ^ index is in elements, not bytes

instance Contractible (CompactMutableArray s a) where
  unsafeSizeOfContractedElement _ = sizeOf (undefined :: Addr)
  unsafeWriteContractedArray (ContractedMutableArray marr) ix (CompactMutableArray (MutablePrimArray arr#)) = 
    writeByteArray marr ix (unsafeUnliftedToAddr arr#)
  unsafeReadContractedArray (ContractedMutableArray marr) ix = do
    addr <- readByteArray marr ix
    return (CompactMutableArray (MutablePrimArray (unsafeUnliftedFromAddr addr)))
  


