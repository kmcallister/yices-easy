import Yices.Easy
import Yices.Easy.Sugar
import Yices.Easy.Build

import Control.Monad
import System
import Data.List

import qualified Data.Map      as M
import qualified Data.GraphViz as G

type Edge  = (Ident, Ident)
data Graph = Graph [Ident] [Edge]

-- graph file contains lines with edges as:
-- nodeA nodeB
parse :: String -> Graph
parse g = Graph vs es where
  es = map ((\[x,y] -> (x,y)) . words) $ lines g
  vs = nub $ concat [ [x,y] | (x,y) <- es ]

query :: Int -> Graph -> Query
query n (Graph vs es) = execBuild $ do
  forM_ vs $ \v -> do
    x <- declInt v
    assert ((x >=. 0) &&. (x <. fromIntegral n))
  forM_ es $ \(x,y) -> assert (Var x /=. Var y)

render :: Graph -> Model -> String
render (Graph vs es) m = G.printDotGraph g where
  g   = G.DotGraph False False Nothing $ G.DotStmts gbl [] vss ess
  gbl = [G.NodeAttrs [G.Style [G.SItem G.Filled []]]]
  vss = [G.DotNode v [G.Color [G.X11Color $ color v]] | v <- vs]
  ess = [G.DotEdge x y False [] | (x,y) <- es]
  colors = [G.Red, G.Green, G.Blue, G.Cyan, G.Magenta, G.Yellow, G.White]
  color v = case M.lookup v m of Just (ValInt i) -> colors !! fromIntegral i

-- runhaskell graph-color.hs <number of colors> <graph file>
main :: IO ()
main = do
  [nx,file] <- getArgs
  numColors <- readIO nx
  graph     <- parse `fmap` readFile file
  result    <- solve $ query numColors graph
  case result of
    Sat model -> writeFile "out.dot" $ render graph model
    _         -> putStrLn "No solution." >> exitFailure
