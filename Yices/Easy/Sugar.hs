-- | Syntactic sugar for embedding Yices expressions in Haskell.
--
-- Defines a number of aliases and infix operators, plus a
-- @'Num'@ instance for @'Expr'@.
--
-- This is not an essential part of the interface. Expressions
-- may be built using @Yices.Easy.Types@ directly.
--
-- This is a quick hack.  There are several other projects working
-- on vastly more sophisticated SMT EDSLs, so this one is unlikely
-- to receive much attention.  That said, suggestions and patches
-- are always welcome.

{-# OPTIONS_GHC
  -fno-warn-orphans #-}

module Yices.Easy.Sugar
 ( -- * Types
   real, int, nat, bool

   -- * Booleans
 , true, false
 , (||.), (&&.)

   -- * Application
 , ($.)

   -- * Scalar arithmetic
 , add, mul, sub

   -- * Scalar comparison
 , (==.), (/=.), (>.), (>=.), (<.), (<=.)

   -- * Literal bitvectors
 , bv

   -- * Bitvector arithmetic
 , (+@), (-@), (*@)

   -- * Signed bitvector comarpsion
 , (>@), (>=@), (<@), (<=@)
 
   -- * Unsigned bitvector comparison
 , (>@@), (>=@@), (<@@), (<=@@)

   -- * Bitvector concatenation
 , (++@)

   -- * Bitwise operations
 , (|@), (&@), (^@)
 ) where


import Yices.Easy.Types


real, int, nat, bool :: Type
real = TyName "real"
int  = TyName "int"
nat  = TyName "nat"
bool = TyName "bool"


true, false :: Expr
true  = LitBool True
false = LitBool False


infixr 0 $.
($.) :: Expr -> [Expr] -> Expr
($.) = Apply


instance Num Expr where
  x + y = Arith Add [x,y]
  x * y = Arith Mul [x,y]
  x - y = Arith Sub [x,y]
  abs    = error "Expr: cannot use abs"
  signum = error "Expr: cannot use signum"
  fromInteger = LitNum . FromString . show

add, mul, sub :: [Expr] -> Expr
add = Arith Add
mul = Arith Mul
sub = Arith Sub


infix 4 ==.
infix 4 /=.
infix 4 >.
infix 4 >=.
infix 4 <.
infix 4 <=.

(==.), (/=.), (>.), (>=.), (<.), (<=.) :: Expr -> Expr -> Expr
(==.) = Compare Eq
(/=.) = Compare Ne
(>.)  = Compare Gt
(>=.) = Compare Ge
(<.)  = Compare Lt
(<=.) = Compare Le


infixr 2 ||.
infixr 3 &&.

(||.), (&&.) :: Expr -> Expr -> Expr
x ||. y = Logic Or  [x,y]
x &&. y = Logic And [x,y]


infixl 6 +@
infixl 6 -@
infixl 7 *@

(+@), (-@), (*@) :: Expr -> Expr -> Expr
(+@) = BitArith Add
(*@) = BitArith Mul
(-@) = BitArith Sub


infix 4 >@
infix 4 >=@
infix 4 <@
infix 4 <=@
infix 4 >@@
infix 4 >=@@
infix 4 <@@
infix 4 <=@@

(>@), (>=@), (<@), (<=@), (>@@), (>=@@), (<@@), (<=@@) :: Expr -> Expr -> Expr
(>@)   = BitCompare Signed   Gt
(>=@)  = BitCompare Signed   Ge
(<@)   = BitCompare Signed   Lt
(<=@)  = BitCompare Signed   Le
(>@@)  = BitCompare Unsigned Gt
(>=@@) = BitCompare Unsigned Ge
(<@@)  = BitCompare Unsigned Lt
(<=@@) = BitCompare Unsigned Le


infixr 5 ++@

(++@) :: Expr -> Expr -> Expr
(++@) = BitConcat


infixr 5 |@
infixr 7 &@
infixr 7 ^@

(|@), (&@), (^@) :: Expr -> Expr -> Expr
(|@) = BitLogic Or
(&@) = BitLogic And
(^@) = BitLogic Xor


-- | @bv s n@ has value @n@ and size at least @s@.
bv :: Size -> Integer -> Expr
bv _ n
  | n < 0 = error "Yices.Easy.Sugar.bv: negative Integer not allowed"
bv s n = LitBitvec . FromBits $ getBits s n where
  getBits b 0 = replicate b B0
  getBits b k = let (d,m) = k `divMod` 2 in toEnum (fromIntegral m) : getBits (b-1) d
