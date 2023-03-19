module Record where

record Pair (A B : Set) : Set where
  field
    fst : A
    snd : B