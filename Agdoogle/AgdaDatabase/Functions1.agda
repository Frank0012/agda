module Functions1 where

open import Builtin


simple : Bool -> Bool -> Bool
simple true   _  = true
simple false  _  = false
