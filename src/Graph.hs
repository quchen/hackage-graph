-- | Simple graph type.

module Graph where

import Text.Printf
import Data.List

-- | A 'Graph' is defined by its nodes and the targets of its outgoing edges.
newtype Graph = Graph [(String, [String])]

-- | Convert a graph to .dot format
toDot :: [String] -- ^ Packages to ignore
      -> Graph
      -> String
toDot ignore (Graph g) = boilerplate (foldr toEdge "" g) where

      boilerplate :: String -> String
      boilerplate = printf "digraph HackageGraph {\n%s}\n"

      toEdge :: (String, [String]) -- Current node
             -> String -- Rest of the graph, converted to string
             -> String
      toEdge (pName, pDeps) rest
            | pName `elem` ignore = rest
            | otherwise = edge ++ rest
            where edge = printf "\t%s -> { %s };\n" source targets
                  source = quote pName
                  targets = (intercalate "; "
                            . map quote
                            . filter (`notElem` ignore)) pDeps
                  quote = printf "\"%s\""

-- | Number of nodes in the graph.
size :: Graph -> Int
size (Graph g) = length g