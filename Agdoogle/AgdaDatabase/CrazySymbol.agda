module CrazySymbol where

open import Builtin

_≐_ : Bool -> Bool -> Bool
true ≐ true = true
false ≐ false = false
true ≐ false = true
false ≐ true = false

