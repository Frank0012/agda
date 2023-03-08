module nothing where

open import Agda.Primitive
open import Builtin

data rubbish : Set where
   rub  : rubbish
   bish : rubbish -> rubbish


if_then_else_ : {A : Set} → Bool → A → A → A
if true  then x else y = x
if false then x else y = y




postulate
   notVeryUseful : Bool
   evenLessUseful : Set
   LeastUseful : Nat

data whatever : Set where
   wh : whatever
   a  : whatever
   t  : whatever

uselessBooleanFunc : Bool
uselessBooleanFunc = false




data something : Set where
   so  : something
   me  : something
   th  : something
   ing : something
