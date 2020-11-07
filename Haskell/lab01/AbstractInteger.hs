{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module AbstractInteger where

data AbstractNatural = Zero | S AbstractNatural
    deriving (Show)

addNat :: AbstractNatural -> AbstractNatural -> AbstractNatural
addNat x Zero  = x
addNat x (S y) = S (addNat x y)

data AbstractInteger = Pos AbstractNatural | Neg AbstractNatural
    deriving (Show)

successor :: AbstractInteger -> AbstractInteger
successor (Pos x)       = Pos (S x)
successor (Neg Zero)    = Pos (S Zero)
successor (Neg (S x))   = Neg x

predecessor :: AbstractInteger -> AbstractInteger
predecessor (Neg x)     = Neg (S x)
predecessor (Pos Zero)  = Neg (S Zero)
predecessor (Pos (S x)) = Pos x

negator :: AbstractInteger -> AbstractInteger
negator  (Pos x)  = Neg x
negator  (Neg x)  = Pos x

absolute :: AbstractInteger -> AbstractInteger
absolute (Pos x)  = Pos x
absolute (Neg x)  = Pos x

add :: AbstractInteger -> AbstractInteger -> AbstractInteger
add x (Pos Zero)  = x
add x (Neg Zero)  = x
add x (Pos (S y)) = successor (add x (Pos y))
add x (Neg (S y)) = predecessor (add x (Neg y))

difference :: AbstractInteger -> AbstractInteger -> AbstractInteger
difference x y = add x (negator y)

multiply :: AbstractInteger -> AbstractInteger -> AbstractInteger
multiply x (Pos Zero) = Pos Zero
multiply x (Neg Zero) = Neg Zero
multiply x (Pos (S y)) = add x (multiply x (Pos y))
multiply x (Neg (S y)) = negator (multiply x (Pos (S y)))

instance Eq AbstractInteger where
    Pos Zero  == Pos Zero  = True
    Neg Zero  == Neg Zero  = True
    Pos Zero  == Neg Zero  = True
    Neg Zero  == Pos Zero  = True
    Pos Zero  == Pos _     = False
    Pos _     == Pos Zero  = False
    Neg Zero  == Pos _     = False
    Pos _     == Neg Zero  = False
    Neg a     == Pos b     = False
    Pos a     == Pos b     = difference (Pos a) (Pos b) == Pos Zero
    Neg a     == Neg b     = difference (Pos a) (Pos b) == Pos Zero
    _         == _         = False

instance Ord AbstractInteger where
    Pos Zero  <= Pos Zero   = True
    Pos Zero  <= Neg Zero   = True
    Neg Zero  <= Pos Zero   = True
    Neg Zero  <= Neg Zero   = True
    Pos Zero  <= Pos a      = True
    Pos a     <= Pos Zero   = False
    Neg a     <= Neg Zero   = True
    Neg a     <= Pos Zero   = True
    Neg (S x) <= Pos (S y)  = True
    Pos x     <= Pos y      = if difference (Pos x) (Pos y) <= Pos Zero then True else if Pos Zero <= difference (Pos x) (Pos y) then False else False
    Neg x     <= Neg y      = if difference (Neg x) (Neg y) <= Pos Zero then True else if Pos Zero <= difference (Neg x) (Neg y) then False else False
    _ <= _                  = False

divide :: AbstractInteger -> AbstractInteger -> AbstractInteger
divide a b
   | b > a && a > zero  = zero
   | a > b && a < zero  = one
   | a == b = one
   | b < a && b > zero  = add (one) (divide (difference a b) b)
   | a < b && b < zero  = add (one) (divide (difference a b) b)
   | b < a && b < zero && a > zero = add (negativeOne) (divide (add a b) b)
   | a < b && a < zero && b > zero = add (negativeOne) (divide (add a b) b)

modulo :: AbstractInteger -> AbstractInteger -> AbstractInteger
modulo a b = difference a (multiply b (divide a b))

toAbstract :: Integer -> AbstractInteger
toAbstract 0 = Pos Zero
toAbstract x
  |x > 0 = add (one) (toAbstract (x - 1))
  |x < 0 = add (negativeOne) (toAbstract (x + 1))

fromAbstract :: AbstractInteger -> Integer
fromAbstract (Pos Zero) = 0
fromAbstract (Pos x) = 1 + fromAbstract (predecessor (Pos x))
fromAbstract (Neg x) = (- 1) + fromAbstract (successor (Neg x))

evaluateRPN :: [String] -> AbstractInteger
evaluateRPN inputList = evalRPNStack [] inputList

evalRPNStack :: [AbstractInteger] -> [String] -> AbstractInteger
evalRPNStack stack inputList =
    case (stack, inputList) of
        ( x:_,           [] )            -> x -- No more input, return top of stack.
        ( y:x:stackRest, "+":inputRest ) -> evalRPNStack (add x y        : stackRest) inputRest
        ( y:x:stackRest, "-":inputRest ) -> evalRPNStack (difference x y   : stackRest) inputRest
        ( y:x:stackRest, "*":inputRest ) -> evalRPNStack (multiply x y   : stackRest) inputRest
        ( y:x:stackRest, "/":inputRest ) -> evalRPNStack (divide x y   : stackRest) inputRest
        ( y:x:stackRest, "%":inputRest ) -> evalRPNStack (modulo x y   : stackRest) inputRest
        ( x:stackRest, "abs":inputRest ) -> evalRPNStack (absolute x   : stackRest) inputRest
        -- This last case handles numeric inputs, "0" "-2" "34" etc...
        ( _,          numStr:inputRest ) -> evalRPNStack (toAbstract (read numStr) : stack) inputRest


zero  = Pos Zero
one   = successor zero
two   = successor one
three = successor two
four  = successor three
five  = successor four
six   = successor five
seven = successor six
eight = successor seven
nine  = successor eight
ten   = successor nine

negativeOne   = predecessor zero
negativeTwo   = predecessor negativeOne
negativeThree = predecessor negativeTwo
negativeFour  = predecessor negativeThree
negativeFive  = predecessor negativeFour
negativeSix   = predecessor negativeFive
negativeSeven = predecessor negativeSix
negativeEight = predecessor negativeSeven
negativeNine  = predecessor negativeEight
negativeTen   = predecessor negativeNine
