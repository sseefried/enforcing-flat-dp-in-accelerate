{-# LANGUAGE GADTs, StandaloneDeriving, RankNTypes, FlexibleInstances, EmptyDataDecls #-}
--
-- In this module I'm attempting to see if we can enforce flat data parallelism
-- in a language like Accelerate by using fancy types. We have a problem in accelerate that you can 
-- write programs like this:
--
-- accUsingLambda :: Acc (Acc.Vector Int)
-- accUsingLambda = Acc.map (\i -> generate (index1 i) (const 42) ! (index1 0)) vec
--   where vec = mkVec 1
--
-- This is nested data parallelism.
--
-- We can enforce flat data parallelism with 
--
module Main where

import Control.Monad.ST
import Data.STRef

data Array a = AnArray -- just a tag

data Acc s a where
  Map      :: (forall t.Exp t a -> Exp t b) -> Acc s (Array a) -> Acc s (Array b)
  Use      :: Array a -> Acc s (Array a)



data Exp s a where
  PrimApp     :: PrimFun -> Exp s (Int,Int) -> Exp s Int
  Tup2        :: (Exp s Int, Exp s Int) -> Exp s (Int,Int)
  Const       :: Int -> Exp s Int
  IndexScalar :: Acc s (Array a) -> Exp s Int -> Exp s a

data PrimFun = APrimFun

instance Show (Exp s a) where
  show = error "not defined"

instance Eq (Exp s a) where
 (==) = error "not defined"

instance Num (Exp s Int) where
  fromInteger n = Const (fromIntegral n)
  a + b         = PrimApp APrimFun (Tup2 (a,b))
  (*)           = error "not defined"
  abs           = error "not defined"
  signum        = error "not defined"

arrInt = Use (AnArray :: Array Int)

allow  = Map (\x -> x + 1) arrInt
allow2 = Map (\x -> IndexScalar (Map (\y -> y + 1) arrInt) x) arrInt

-- This won't type check.
disallow = Map (\x -> IndexScalar (Map (\y -> x + y ) arrInt) x) arrInt