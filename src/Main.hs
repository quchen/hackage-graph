module Main where

import Text.Printf
import System.IO

import Graph
import MakeGraph



-- | Path to 00-index.tar
packageDB :: FilePath
packageDB = "/home/main/.cabal/packages/hackage.haskell.org/00-index.tar"



-- | Packages to be ignored
ignore :: [String]
ignore = ["base"]



-- | Read package DB, write dot-formatted graph to STDOUT
main :: IO ()
main = do
      graph <- makeGraph packageDB
      _ <- hPrintf stderr "Graph size: %d nodes\n" (size graph)
      putStrLn (toDot ignore graph)
