{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where



import qualified Data.Map           as M
import           Data.Text.IO       as T (putStrLn)
import           System.Environment
import           System.IO
import           Text.Printf

import Graph
import PackageGraph



-- | Read package DB, write dot-formatted graph to STDOUT
main :: IO ()
main = do
    packageDb <- lookupEnv "HOME" >>= \case
        Just h -> pure (h ++ "/.stack/indices/Hackage/00-index.tar")
        Nothing -> error "$HOME not set"
    graph <- makeGraph packageDb
    let (nodes, edges) = size graph
    _ <- hPrintf stderr "Graph size: %d nodes, %d edges\n" nodes edges
    -- T.putStrLn (toDot graph)
    let Graph g = graph
    print (transitiveClosure (Graph (M.delete (Labeled undefined "acme-everything") g)))
