-- | Simple graph type.

{-# LANGUAGE OverloadedStrings #-}

module Graph where

import           Data.Set      (Set)
import qualified Data.Set      as Set
import           Data.Map      (Map)
import qualified Data.Map      as Map
import           Data.Text     (Text)
import qualified Data.Text     as T
import qualified Data.Foldable as F
import           Data.Monoid



-- | A 'Graph' is defined by its nodes and the targets of its outgoing edges.
newtype Graph = Graph (Map Text (Set Text))

-- | Convert a graph to .dot format
toDot :: Set Text -- ^ Packages to ignore
      -> Graph
      -> Text
toDot ignore (Graph g) = boilerplate (Map.foldMapWithKey toEdge g) where

      boilerplate :: Text -> Text
      boilerplate x = "digraph HackageGraph {\n" <> x <> "}\n"

      toEdge :: Text     -- ^ Node name
             -> Set Text -- ^ Target nodes ("depends on")
             -> Text     -- ^ "Source -> { Target1; Target2; ... }"
      toEdge node edges | node `Set.member` ignore = mempty
                        | otherwise = mconcat [ "\t"
                                              , quote node
                                              , " -> { "
                                              , toText edges
                                              , " }"
                                              ]

      toText :: Set Text -> Text
      toText s = ( T.intercalate ", "
                 . map quote
                 . filter (`Set.notMember` ignore)
                 . Set.toList
                 ) s

      quote :: Text -> Text
      quote x = "\"" <> x <> "\""



-- | Number of (nodes, edges) in the graph.
size :: Graph -> (Int, Int)
size (Graph g) = (nodes, edges) where
      nodes = Map.size g
      edges = getSum (F.foldMap (Sum . Set.size) g)