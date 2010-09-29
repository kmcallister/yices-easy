import Yices.Easy
import Yices.Easy.Sugar
import Yices.Easy.Build

import Control.Monad ( forM_, liftM2 )

import qualified Data.Map as M

import System

cell :: (Int,Int) -> String
cell (x,y) = concat ["c", show x, "_", show y]

query :: Int -> Query
query n = execBuild $ do
  let cells = liftM2 (,) [1..n] [1..n]
  forM_ cells $ \c -> do
    x <- declInt $ cell c
    assert ((x >=. 0) &&. (x <. fromIntegral n))

  forM_ cells $ \c@(i0,j0) -> do
    let notEq c1 = assert (Var (cell c) /=. Var (cell c1))
    forM_ [i0+1..n] $ \i -> notEq (i, j0)
    forM_ [j0+1..n] $ \j -> notEq (i0,j )

run :: Int -> IO ()
run n = do
  Sat model <- solve $ query n
  let soln c = case M.lookup (cell c) model of Just (ValInt k) -> fromIntegral k
      chars  = ['A'..'Z']
      line i = forM_ [1..n] $ \j -> putChar (chars !! soln (i,j)) >> putChar ' '
  forM_ [1..n] $ \i -> line i >> putChar '\n'

-- generate latin squares
-- runhaskell latin.hs <size of square>
main :: IO ()
main = do
  [n] <- getArgs
  nn  <- readIO n
  run nn
