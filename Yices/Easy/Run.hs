{-# LANGUAGE
    PatternGuards
  , Rank2Types #-}

-- | Running the Yices SMT solver.

module Yices.Easy.Run
  ( -- * Solving
    check
  , checkIO
    -- * Debugging
  , dump
  ) where

import Yices.Easy.Types

import Data.Ratio
import Control.Applicative
import Control.Monad
import Control.Exception
import Foreign
import Foreign.C

import qualified Data.Map              as M
import qualified Foreign.Marshal.Utils as MU

import qualified Bindings.Yices.Internal as Y

type PExpr  = Ptr Y.YExpr
type PType  = Ptr Y.YType
type PCtx   = Ptr Y.YContext
type PVar   = Ptr Y.YVarDecl
type PModel = Ptr Y.YModel

type Env = M.Map Ident PVar

fI :: (Integral a, Num b) => a -> b
fI = fromIntegral

buildExpr :: Env -> PCtx -> Expr -> IO PExpr
buildExpr env ctx = go where
  lift1 x     f = go x >>= f
  lift2 x y   f = join $ liftM2 f (go x) (go y)
  lift3 x y z f = join $ liftM3 f (go x) (go y) (go z)
  liftN xs f = do
    ys <- mapM go xs
    withArrayLen ys $ \n a -> f a (fI n)

  go (LitBool b) = (if b then Y.c_mk_true else Y.c_mk_false) ctx
  go (LitNum (FromInt    i)) = Y.c_mk_num ctx $ fI i
  go (LitNum (FromString s)) = withCString s $ Y.c_mk_num_from_string ctx

  go (Var i)
    | Just d <- M.lookup i env = Y.c_mk_var_from_decl ctx d
    | otherwise                = error ("undefined variable: " ++ i)

  go (Apply   f xs ) = lift1 f $ \fp -> liftN xs $ Y.c_mk_app ctx fp
  go (Arith   o es ) = liftN es $ getArith o ctx
  go (Logic   o es ) = liftN es $ getLogic o ctx
  go (Compare o x y) = lift2 x y $ getCompare o ctx
  go (Not       x  ) = lift1 x $ Y.c_mk_not ctx

  go (IfThenElse x y z) = lift3 x y z $ Y.c_mk_ite ctx

  go (LitBitvec (FromULong n v)) = Y.c_mk_bv_constant ctx (fI n) v
  go (LitBitvec (FromBits bs)) = withArrayLen (map (fI.fromEnum) bs) $
    \n xs -> Y.c_mk_bv_constant_from_array ctx (fI n) xs

  go (BitArith     o x y) = lift2 x y $ getBArith     o  ctx
  go (BitLogic     o x y) = lift2 x y $ getBLogic     o  ctx
  go (BitCompare s o x y) = lift2 x y $ getBCompare s o  ctx
  go (BitMinus       x  ) = lift1 x   $ Y.c_mk_bv_minus  ctx
  go (BitNot         x  ) = lift1 x   $ Y.c_mk_bv_not    ctx
  go (BitConcat      x y) = lift2 x y $ Y.c_mk_bv_concat ctx

  go (BitShift d f  n  x) = lift1 x $ \p -> getBShift d f ctx p (fI n)
  go (BitExtract n0 n1 x) = lift1 x $ Y.c_mk_bv_extract ctx (fI n1) (fI n0)
  go (BitSignEx     n  x) = lift1 x $ \p -> Y.c_mk_bv_sign_extend ctx p (fI n)

  getArith Add = Y.c_mk_sum
  getArith Sub = Y.c_mk_sub
  getArith Mul = Y.c_mk_mul

  getLogic And = Y.c_mk_and
  getLogic Or  = Y.c_mk_or
  getLogic Xor = error "no xor for booleans"

  getCompare Eq = Y.c_mk_eq
  getCompare Ne = Y.c_mk_diseq
  getCompare Gt = Y.c_mk_gt
  getCompare Ge = Y.c_mk_ge
  getCompare Lt = Y.c_mk_lt
  getCompare Le = Y.c_mk_le

  getBArith Add = Y.c_mk_bv_add
  getBArith Sub = Y.c_mk_bv_sub
  getBArith Mul = Y.c_mk_bv_mul

  getBLogic And = Y.c_mk_bv_and
  getBLogic Or  = Y.c_mk_bv_or
  getBLogic Xor = Y.c_mk_bv_xor

  getBCompare _ Eq = Y.c_mk_eq
  getBCompare _ Ne = Y.c_mk_diseq
  getBCompare Signed   Gt = Y.c_mk_bv_sgt
  getBCompare Signed   Ge = Y.c_mk_bv_sge
  getBCompare Signed   Lt = Y.c_mk_bv_slt
  getBCompare Signed   Le = Y.c_mk_bv_sle
  getBCompare Unsigned Gt = Y.c_mk_bv_gt
  getBCompare Unsigned Ge = Y.c_mk_bv_ge
  getBCompare Unsigned Lt = Y.c_mk_bv_lt
  getBCompare Unsigned Le = Y.c_mk_bv_le

  getBShift L B0 = Y.c_mk_bv_shift_left0
  getBShift L B1 = Y.c_mk_bv_shift_left1
  getBShift R B0 = Y.c_mk_bv_shift_right0
  getBShift R B1 = Y.c_mk_bv_shift_right1

buildType :: PCtx -> Type -> IO PType
buildType ctx = go where
  lift1 x     f = go x >>= f
  liftN xs f = do
    ys <- mapM go xs
    withArrayLen ys $ \n a -> f a (fI n)

  go (TyName xs) = withCString xs $ Y.c_mk_type ctx
  go (TyFun xs r) = lift1 r $ \rp -> liftN xs $ \xp n ->
    Y.c_mk_function_type ctx xp n rp
  go (TyBitvec n) = Y.c_mk_bitvector_type ctx (fI n)
  go (TyTuple xs) = liftN xs $ \xp n -> MU.with xp $ \xpp ->
    Y.c_mk_tuple_type ctx xpp n

withContext :: Context -> (Env -> PCtx -> IO a) -> IO a
withContext (Context ds as) act
    = bracket Y.c_mk_context Y.c_del_context $ \ctx -> do
  let mkD m (Declare x t) = do
        tp <- buildType ctx t
        dp <- withCString x $ \xp -> Y.c_mk_var_decl ctx xp tp
        return $ M.insert x dp m
  env <- foldM mkD M.empty ds
  let mkA (Assert e) =
        buildExpr env ctx e >>= Y.c_assert ctx
        {-
      mkA (AssertWeighted w e) =
        buildExpr env ctx e >>= flip (Y.c_assert_weighted ctx) (fI w)
          >> return () -}
  mapM_ mkA as
  act env ctx 

-- | Dump some information about a context
-- and expression to standard output.
dump :: Context -> Expr -> IO ()
dump c e = withContext c $ \env ctx -> do
  ep <- buildExpr env ctx e
  Y.c_dump_context ctx
  putStrLn "expression:"
  Y.c_pp_expr ep
  putStrLn ""

get :: Env -> PModel -> Get -> IO Got
get env mdl (Get v t) = Got v <$> go (M.lookup v env) where
  go (Just dp) = case t of
    VarBool     -> fromLBool <$>   Y.c_get_value mdl dp
    VarInt      -> chk ValInt    $ Y.c_get_int_value       mdl dp
    VarRational -> withRational  $ Y.c_get_arith_value     mdl dp
    VarDouble   -> chk ValDouble $ Y.c_get_double_value    mdl dp
    VarBitvec n -> withVec n     $ Y.c_get_bitvector_value mdl dp (fI n)
  go Nothing = return ValUnknown

  fromLBool 1 = ValBool True
  fromLBool _ = ValBool False

  chk f a = alloca $ \p -> do
    x <- a p
    if x /= 1
      then return ValUnknown
      else f <$> peek p
  withRational a = alloca $ \n -> alloca $ \d -> do
    x <- a n d
    if x /= 1
      then return ValUnknown
      else ValRational <$> liftM2 (%) (fI <$> peek n) (fI <$> peek d)
  withVec n a = allocaArray n $ \p -> do
    x <- a p
    if x /= 1
      then return ValUnknown
      else (ValBitvec . map (toEnum.fI)) <$> peekArray n p

-- | The @'IO'@ action underlying @'check'@.
checkIO :: Context -> ModelType -> IO (Maybe Model)
checkIO c ts = withContext c $ \env ctx -> do
  sat <- Y.c_check ctx
  if sat /= 1 then return Nothing else do
    mdl <- Y.c_get_model ctx
    Just <$> mapM (get env mdl) ts

-- | Check a context for satisfiability.
--
-- If satisfiable, returns values for the specified
-- variables.
--
-- Here we assume that Yices, taken as a whole, is
-- a pure function.  We wrap the IO action returned
-- by @'checkIO'@ using @'unsafePerformIO'@.
check :: Context -> ModelType -> Maybe Model
check ctx ts = unsafePerformIO $ checkIO ctx ts
