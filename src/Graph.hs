-- | Simple graph type.

{-# LANGUAGE OverloadedStrings #-}

module Graph (
    toDot,
    Graph(..),
    Labeled(..),
    size,
    transitiveClosure,
) where



import           Data.Map    (Map)
import qualified Data.Map    as Map
import           Data.Monoid
import           Data.Set    (Set)
import qualified Data.Set    as Set
import           Data.Text   (Text)
import qualified Data.Text   as T



data Labeled l a = Labeled l a
    deriving (Show)

instance Eq a => Eq (Labeled l a) where
    Labeled _ x == Labeled _ y = x == y

instance Ord a => Ord (Labeled l a) where
    Labeled _ x `compare` Labeled _ y = compare x y

unLabel :: Labeled l a -> a
unLabel (Labeled _ x) = x

-- | A 'Graph' is defined by its nodes and the targets of its outgoing edges.
newtype Graph n e = Graph (Map (Labeled n Text) (Labeled e (Set Text)))

-- | Convert a graph to .dot format
toDot :: Graph n e -> Text
toDot (Graph g) = boilerplate (Map.foldMapWithKey toEdge g) where

      boilerplate :: Text -> Text
      boilerplate x = "digraph HackageGraph {\n" <> x <> "}\n"

      toEdge :: Labeled n Text     -- ^ Node name
             -> Labeled e (Set Text) -- ^ Target nodes ("depends on")
             -> Text     -- ^ "Source -> { Target1; Target2; ... }"
      toEdge (Labeled _ node) (Labeled _ edges)
        = mconcat [ "\t"
                  , quote node
                  , " -> { "
                  , toText edges
                  , " }\n"
                  ]

      toText :: Set Text -> Text
      toText s = ( T.intercalate ", "
                 . map quote
                 . Set.toList
                 ) s

      quote :: Text -> Text
      quote x = "\"" <> x <> "\""

-- | Number of (nodes, edges) in the graph.
size :: Graph n e -> (Int, Int)
size (Graph g) = (nodes, edges) where
      nodes = Map.size g
      edges = getSum (foldMap (Sum . Set.size . unLabel) g)

-- | Set of all nodes from with the entire graph is reachable.
-- Good for generating acme-everything. :-)
transitiveClosure :: Graph n e -> Set Text
transitiveClosure (Graph g) = Set.map unLabel (Map.keysSet g) `Set.difference` foldMap unLabel g
