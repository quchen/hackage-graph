{-# LANGUAGE OverloadedStrings #-}

module Main where



import Data.Set     (Set, fromList)
import Data.Text    (Text)
import Data.Text.IO as T (putStrLn)
import System.IO
import Text.Printf

import Graph
import MakeGraph



-- | Path to 00-index.tar
packageDB :: FilePath
packageDB = "/home/main/.cabal/packages/hackage.haskell.org/00-index.tar"



-- | Packages to be ignored
ignore :: Set Text
ignore = fromList ["base"]



-- | Read package DB, write dot-formatted graph to STDOUT
main :: IO ()
main = do
      graph <- makeGraph packageDB
      let (nodes, edges) = size graph
      _ <- hPrintf stderr "Graph size: %d nodes, %d edges\n" nodes edges
      T.putStrLn (toDot ignore graph)
