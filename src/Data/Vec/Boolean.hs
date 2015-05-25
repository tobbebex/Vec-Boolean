{-# LANGUAGE TypeOperators, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, FlexibleContexts, UndecidableInstances #-}
-----------------------------------------------------------------------------
--
-- Module      :  Data.Vec.Boolean
-- Copyright   :  Tobias Bexelius
-- License     :  BSD3
--
-- Maintainer  :  Tobias Bexelius
-- Stability   :
-- Portability :
--
-- |
-- Besides the two functions listed below, this module provides 'Data.Boolean.IfB', 'Data.Boolean.EqB'
-- and 'Data.Boolean.OrdB' instances for the @()@ and the @:.@ data types.
--
-- Two fixed function lists are considered equal if all elements are equal. For element-wise comparisions,
-- use the 'all' and 'any' functions.
-----------------------------------------------------------------------------

module Data.Vec.Boolean (
    all,
    any,
) where

import Data.Boolean
import Data.Vec
import Prelude hiding (any, all)

type instance BooleanOf (a:.b) = BooleanOf a    

instance IfB a => IfB (a:.()) where
    ifB bool (a1:.()) (a2:.()) = ifB bool a1 a2 :. ()
instance (bool ~ BooleanOf a, bool ~ BooleanOf (b:.()), Boolean bool, IfB a, IfB (b:.())) => IfB (a:.b:.()) where
    ifB bool (a1:.b1) (a2:.b2) = ifB bool a1 a2 :. ifB bool b1 b2
instance (bool ~ BooleanOf a, bool ~ BooleanOf (b:.c:.()), Boolean bool, IfB a, IfB (b:.c:.())) => IfB (a:.b:.c:.()) where
    ifB bool (a1:.b1) (a2:.b2) = ifB bool a1 a2 :. ifB bool b1 b2
instance (bool ~ BooleanOf a, bool ~ BooleanOf (b:.c:.d:.()), Boolean bool, IfB a, IfB (b:.c:.d:.())) => IfB (a:.b:.c:.d:.()) where
    ifB bool (a1:.b1) (a2:.b2) = ifB bool a1 a2 :. ifB bool b1 b2

instance EqB a => EqB (a:.()) where
    (a1:.()) ==* (a2:.()) = a1==*a2
    (a1:.()) /=* (a2:.()) = a1/=*a2
instance (bool ~ BooleanOf a, bool ~ BooleanOf (b:.()), Boolean bool, EqB a, EqB (b:.())) => EqB (a:.b:.()) where
    (a1:.b1) ==* (a2:.b2) = a1==*a2 &&* b1==*b2
    (a1:.b1) /=* (a2:.b2) = a1/=*a2 ||* b1/=*b2
instance (bool ~ BooleanOf a, bool ~ BooleanOf (b:.c:.()), Boolean bool, EqB a, EqB (b:.c:.())) => EqB (a:.b:.c:.()) where
    (a1:.b1) ==* (a2:.b2) = a1==*a2 &&* b1==*b2
    (a1:.b1) /=* (a2:.b2) = a1/=*a2 ||* b1/=*b2
instance (bool ~ BooleanOf a, bool ~ BooleanOf (b:.c:.d:.()), Boolean bool, EqB a, EqB (b:.c:.d:.())) => EqB (a:.b:.c:.d:.()) where
    (a1:.b1) ==* (a2:.b2) = a1==*a2 &&* b1==*b2
    (a1:.b1) /=* (a2:.b2) = a1/=*a2 ||* b1/=*b2

-- | Evaluates to 'true' if all elements in the fixed length list is 'true'
all :: (Boolean bool, Fold v bool) => v -> bool
all = fold (&&*)

-- | Evaluates to 'false' if all elements in the fixed length list is 'false'
any :: (Boolean bool, Fold v bool) => v -> bool
any = fold (||*)

