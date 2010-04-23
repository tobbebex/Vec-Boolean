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

instance Boolean bool => IfB bool () where
    ifB _ _ _ = ()

instance (IfB bool a, IfB bool b) => IfB bool (a:.b) where
    ifB bool (a1:.b1) (a2:.b2) = ifB bool a1 a2 :. ifB bool b1 b2

instance Boolean bool => EqB bool () where
    _ ==* _ = true
    _ /=* _ = false

instance (EqB bool a, EqB bool b) => EqB bool (a:.b) where
    (a1:.b1) ==* (a2:.b2) = a1==*a2 &&* b1==*b2
    (a1:.b1) /=* (a2:.b2) = a1/=*a2 ||* b1/=*b2

-- | Evaluates to 'true' if all elements in the fixed length list is 'true'
all :: (Boolean bool, Fold v bool) => v -> bool
all = fold (&&*)

-- | Evaluates to 'false' if all elements in the fixed length list is 'false'
any :: (Boolean bool, Fold v bool) => v -> bool
any = fold (||*)
