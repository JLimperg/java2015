{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE CPP #-}

module Data.Geometry.SetOps where

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative ((<$>))
#endif

import           Data.Geometry.Shape

-- | Represents two shapes combined by a binary operator. The operator
-- determines whether a point lies within the combined shape, depending on
-- whether it lies within the two constituent shapes.
data Op2 shape = Op2 (Bool -> Bool -> Bool) shape shape
  deriving (Functor)

instance (Shape shape) => Shape (Op2 shape) where
    contains (Op2 f operand1 operand2) point =
      f (contains operand1 point) (contains operand2 point)
    offset op2 vec = flip offset vec <$> op2

mkIntersection :: s -> s -> Op2 s
mkIntersection = Op2 (&&)

mkUnion :: s -> s -> Op2 s
mkUnion = Op2 (||)

mkComplement :: s -> s -> Op2 s
mkComplement = Op2 $ \contained1 contained2 -> contained1 && not (contained2)
