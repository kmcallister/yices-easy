{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , StandaloneDeriving
  , NamedFieldPuns #-}

-- | Provides a monad transformer for constructing Yices queries.
--
-- This is not an essential part of the interface.  Expressions
-- may be built using @Yices.Easy.Types@ only.  The monad is simply
-- a way to handle bookkeeping regarding fresh names, accumulated
-- declarations \/ assertions, and choosing variables to retrieve
-- from the @'Model'@.

module Yices.Easy.Build
  ( -- * The monad
    BuildT
  , Build
  , runBuildT
  , execBuildT
  , runBuild
  , execBuild

    -- * Invoking Yices directly
  , checkBuild

    -- * Fresh names
  , freshName

    -- * Declarations
  , declare
  , fresh

    -- * Assertions
  , assert

    -- * Getting variables
  , get

    -- * Declaring and getting
  , declInt
  , declBool
  , declBitvec
  ) where

import Yices.Easy.Types
import Yices.Easy.Run

import Data.Functor.Identity
import Control.Monad
import Control.Applicative

import qualified Control.Monad.Trans.State as S

data State = State
  { sNext    :: Integer
  , sDecls   :: [Declaration]
  , sAsserts :: [Assertion]
  , sGets    :: [Get] }

startState :: State
startState = State
  { sNext    = 0
  , sDecls   = []
  , sAsserts = []
  , sGets    = [] }

-- | Monad transformer for building Yices queries.
newtype BuildT m a = BuildT (S.StateT State m a)

type Build a = BuildT Identity a

deriving instance (Functor m) => Functor (BuildT m)
deriving instance (Monad   m) => Monad   (BuildT m)
deriving instance (Functor m, Monad m) => Applicative (BuildT m)

runBuildT :: (Monad m) => BuildT m a -> m (a, Query)
runBuildT (BuildT x) = do
  (v, State { sDecls, sAsserts, sGets }) <- S.runStateT x startState
  return (v, Query (Context sDecls sAsserts) sGets)

execBuildT :: (Monad m) => BuildT m a -> m Query
execBuildT x = snd `liftM` runBuildT x

runBuild :: Build a -> (a, Query)
runBuild = runIdentity . runBuildT

execBuild :: Build a -> Query
execBuild = runIdentity . execBuildT

-- | Invoke @'check'@ on a @'Build'@ action directly.
checkBuild :: Build a -> Maybe Model
checkBuild = check . execBuild

-- | Generate a fresh name from a base string.
freshName :: (Monad m) => Ident -> BuildT m Ident
freshName x = BuildT $ do
  s@State { sNext=n } <- S.get
  S.put $ s { sNext=succ n }
  return (x ++ ('_' : show n))

-- | Declare a variable.
declare :: (Monad m) => Ident -> Type -> BuildT m Expr
declare x t = BuildT $ do
  S.modify $ \s -> s { sDecls = Declare x t : sDecls s }
  return $ Var x

-- | Declare a fresh variable given a base name.
fresh :: (Monad m) => Ident -> Type -> BuildT m Expr
fresh x t = freshName x >>= flip declare t

-- | Assert a Boolean truth.
assert :: (Monad m) => Expr -> BuildT m ()
assert e = BuildT . S.modify $ \s ->
  s { sAsserts = Assert e : sAsserts s }

-- | Arrange to get this variable in the resulting @'Model'@.
get :: (Monad m) => Ident -> VarType -> BuildT m ()
get x t = BuildT . S.modify $ \s ->
  s { sGets = Get x t : sGets s }

declGet :: (Monad m) => Type -> VarType -> Ident -> BuildT m Expr
declGet t v x = do
  e <- declare x t
  get x v
  return e

-- | Declare an @int@ variable and @'get'@ it.
declInt :: (Monad m) => Ident -> BuildT m Expr
declInt = declGet (TyName "int") VarInt

-- | Declare a @bool@ variable and @'get'@ it.
declBool :: (Monad m) => Ident -> BuildT m Expr
declBool = declGet (TyName "bool") VarBool

-- | Declare a bitvector variable and @'get'@ it.
declBitvec :: (Monad m) => Size -> Ident -> BuildT m Expr
declBitvec n = declGet (TyBitvec n) (VarBitvec n)
