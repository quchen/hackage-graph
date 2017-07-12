{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where



import qualified Data.List                             as L
import qualified Data.Map                              as M
import           Data.Ord
import qualified Data.Set                              as S
import           Data.Text                             (Text)
import qualified Data.Text                             as T
import qualified Data.Text.IO                          as T
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text
import           Data.Time
import           System.Environment

import Graph
import PackageGraph



-- | Read package DB, write dot-formatted graph to STDOUT
main :: IO ()
main = do
    packageDb <- lookupEnv "HOME" >>= \case
        Just h -> pure (h ++ "/.stack/indices/Hackage/00-index.tar")
        Nothing -> error "$HOME not set"
    Graph g <- makeGraph packageDb
    let sources = transitiveClosure (Graph (M.delete (Labeled undefined "acme-everything") g))
    ymd <- fmap (toGregorian . utctDay) getCurrentTime
    T.putStrLn (makeDotCabal ymd (S.toList sources))

makeDotCabal
    :: (Integer, Int, Int) -- ^ Year, month, day
    -> [Text]              -- ^ Dependencies
    -> Text
makeDotCabal (y,m,d) deps = (renderStrict . layoutPretty defaultLayoutOptions { layoutPageWidth = Unbounded} . vsep)
    [ "name:          acme-everything"
    , "version:       " <> pretty y <> dot <> pretty m <> dot <> pretty d
    , "synopsis:      Install everything."
    , "description:"
    , indent 4 (vsep
        [ "This package requires the entirety of Hackage to be built."
        , "."
        , "The joke started when a colleague liked the"
        , "@leftToMaybe :: Either a b -> Maybe a@ function from @either@, which has a"
        , "*huge* footprint compared to the utility it provides."
        , "."
        , "This got us thinking about seemingly small packages that have lots of hidden"
        , "transitive dependencies, e.g. \"only\" @base@, @lens@ and @yesod@. And that's"
        , "where `acme-everything` was born, which (transitively) depends on the"
        , "entirety of Hackage." ])
    , ""
    , "license:       PublicDomain"
    , "author:        David Luposchainsky <dluposchainsky(λ)gmail.com>"
    , "maintainer:    David Luposchainsky <dluposchainsky(λ)gmail.com>"
    , "build-type:    Simple"
    , "homepage:      https://github.com/quchen/acme-everything"
    , "bug-reports:   https://github.com/quchen/acme-everything/issues"
    , "category:      ACME"
    , "cabal-version: >= 1.10"
    , ""
    , "source-repository head"
    , indent 4 (vsep
        [ "type:     git"
        , "location: https://github.com/quchen/acme-everything" ])
    , ""
    , "library"
    , indent 4 (vsep
        [ "exposed-modules:  Acme.Everything"
        , "hs-source-dirs:   src"
        , "default-language: Haskell2010"
        , "build-depends:    base >= 1 && <= 127"
        , indent (length ("build-depends:  " :: String))
                 (vsep (map (\x -> ", " <> pretty x)
                       (L.sortBy (comparing T.toCaseFold) deps) ))])]
