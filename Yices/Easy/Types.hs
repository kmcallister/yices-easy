-- | Types for Yices syntax, in the subset we support.

module Yices.Easy.Types
  -- export everything
  where

import Foreign.C.Types ( CULong, CLong, CDouble )

-- * Top level

-- | Declare the existence of a variable.
data Declaration
  = Declare Ident Type
  deriving (Show)

-- | Assert a Boolean fact.
newtype Assertion
  = Assert Expr
--  AssertWeighted Weight Expr
  deriving (Show)

-- | Assert some facts in the context of some variables.
data Context
  = Context [Declaration] [Assertion]
  deriving (Show)

-- * Expressions

-- | Types.
data Type
  = TyName   Ident        -- ^ Named type.
  | TyFun    [Type] Type  -- ^ Function, with args and result type.
  | TyBitvec Size         -- ^ Bitvector.
  | TyTuple  [Type]       -- ^ Tuple.
  deriving (Show)

-- | Expressions.
data Expr
  = LitBool    Bool                          -- ^ Literal boolean.
  | LitNum     LitNum                        -- ^ Literal number.
  | Var        Ident                         -- ^ Variable.

  | Apply      Expr [Expr]                   -- ^ Function application.
  | Arith      Arith [Expr]                  -- ^ Arithmetic.
  | Logic      Logic [Expr]                  -- ^ Boolean logic operation.
  | Not        Expr                          -- ^ Boolean negation.
  | Compare    Compare Expr Expr             -- ^ Comparison.
  | IfThenElse Expr Expr Expr                -- ^ Conditional expression.

  | LitBitvec  LitBitvec                     -- ^ Literal bitvector.
  | BitArith   Arith Expr Expr               -- ^ Bitvector arithmetic.
  | BitMinus   Expr                          -- ^ Bitvector arithmetic negation.
  | BitConcat  Expr Expr                     -- ^ Concatenate bitvectors.
  | BitLogic   Logic Expr Expr               -- ^ Bitwise logical operation.
  | BitNot     Expr                          -- ^ Bitwise negate.
  | BitShift   Direction Bit Size Expr       -- ^ Bit shift with fill.
  | BitCompare Signedness Compare Expr Expr  -- ^ Bitvector comparison.
  | BitExtract Index Index Expr              -- ^ Extract a subvector.
  | BitSignEx  Size  Expr                    -- ^ Sign-extend /n/ extra bits.
  deriving (Show, Eq)  -- need Eq for Num

-- | Ways of specifying a literal number.
data LitNum
  = FromInt    Int
  | FromString String  -- ^ ASCII digits.
  deriving (Show, Eq)

-- | Ways of specifying a literal bitvector.
data LitBitvec
  = FromULong  Size CULong
  | FromBits   [Bit]         -- ^ LSB first.
  deriving (Show, Eq)

-- | Arithmetic operations.
data Arith
  = Add
  | Sub
  | Mul
  deriving (Show, Eq)

-- | Logical operations.
data Logic
  = And
  | Or
  | Xor -- ^ NB: bitvector only!
  deriving (Show, Eq)

-- | Comparison operations.
data Compare
  = Eq
  | Ne
  | Gt
  | Ge
  | Lt
  | Le
  deriving (Show, Eq)

-- | Direction of shifting.
data Direction
  = L
  | R
  deriving (Show, Eq)

-- | Bits.
data Bit
  = B0
  | B1
  deriving (Show, Enum, Eq)

-- | Signedness of bitvector operations.
data Signedness
  = Signed
  | Unsigned
  deriving (Show, Eq)

-- * Type synonyms

type Ident  = String
type Size   = Int
type Index  = Int
type Weight = Integer

-- * Asking for models

-- | Types of variables which can be queried
-- from a model.
data VarType
  = VarBool
  | VarInt
  | VarRational
  | VarDouble
  | VarBitvec Size
  deriving (Show)

-- | A variable to query from the model.
data Get
  = Get Ident VarType
  deriving (Show)

type ModelType = [Get]

-- | A query to the solver: solve for some variables in a context.
data Query
  = Query Context ModelType

-- | A value produced from a model.
data Value
  = ValBool     Bool
  | ValInt      CLong
  | ValRational Rational
  | ValDouble   CDouble
  | ValBitvec   [Bit] -- ^ LSB first
  | ValUnknown  -- ^ due to underspecification, type mismatch, etc.
  deriving (Show)

-- | A variable together with its value in a model.
data Got
  = Got Ident Value
  deriving (Show)

type Model = [Got]
