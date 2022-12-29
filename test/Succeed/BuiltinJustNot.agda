module BuiltinJustNot where

open import Agda.Primitive

data Bool : Set where
  false : Bool
  true  : Bool

not : Bool -> Bool
not true = false
not false = true
