module Functions3 where

open import Builtin


add' : Nat -> Nat
add' zero = suc zero
add' (suc n) = suc (add' n)