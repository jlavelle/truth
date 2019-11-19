module Heyting where

import Prelude hiding (not, (&&), (||))
import qualified Prelude

-- From the purescript prelude
class HeytingAlgebra a where
  ff :: a
  tt :: a
  implies :: a -> a -> a
  conj :: a -> a -> a
  disj :: a -> a -> a
  not :: a -> a

  equiv :: a -> a -> a
  equiv a b = a `implies` b && b `implies` a

instance HeytingAlgebra Bool where
  ff = False
  tt = True
  implies a b = not a || b
  conj = (Prelude.&&)
  disj = (Prelude.||)
  not = Prelude.not
  equiv = (==)

(||) :: HeytingAlgebra a => a -> a -> a
(||) = disj

infixr 2 ||

(&&) :: HeytingAlgebra a => a -> a -> a
(&&) = conj

infixr 3 &&
