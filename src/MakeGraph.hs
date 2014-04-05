-- | Create a 'Graph' of a Hackage index.

module MakeGraph (makeGraph) where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as EvilHack
import qualified Codec.Archive.Tar as Tar
import Data.List.Split
import Data.Ord
import Data.Maybe
import Text.Printf
import Text.Read
import Data.List
import Data.Traversable
import Control.Applicative
import Control.Monad
import System.FilePath (splitDirectories)

import qualified Distribution.Package                          as Cabal
import qualified Distribution.PackageDescription               as Cabal
import qualified Distribution.PackageDescription.Configuration as Cabal
import qualified Distribution.PackageDescription.Parse         as Cabal
import qualified Distribution.Simple.Compiler                  as Cabal
import qualified Distribution.System                           as Cabal
import qualified Distribution.Version                          as Cabal

import Graph (Graph(..))



-- | A package, represented by a .cabal file.
data Package = Package { name     :: String         -- ^ Package name
                       , version  :: Version        -- ^ Package version
                       , dotCabal :: BSL.ByteString -- ^ Content of .cabal
                       , _path    :: FilePath       -- ^ Path to .cabal
                       }

instance Show Package where
      show (Package n v _c p) = printf "Package %s v%s at %s" n (show v) p

-- | a.b.c.d
data Version = Version [Int]
      deriving (Eq, Ord)

instance Show Version where
      show (Version vs) = (intercalate "." (map show vs))



-- | Extract .tar file contents, and put them into a flat list
getPackages :: (Show e)
            => Tar.Entries e -- ^ Raw tar content
            -> [Package]
getPackages (Tar.Next entry xs) = case Tar.entryContent entry of
      Tar.NormalFile content _size -> case toPackage entry content of
            Just package -> package : getPackages xs
            _otherwise   -> getPackages xs
      _otherwise -> getPackages xs
getPackages Tar.Done = []
getPackages (Tar.Fail e) = error ("tar failed: " ++ show e)



-- | Convert an entry in a tar file to a 'Package'. 'Nothing' if the file is
--   not a .cabal.
toPackage :: Tar.Entry      -- ^ Tar file 'Tar.Entry'
          -> BSL.ByteString -- ^ File contents
          -> Maybe Package
toPackage entry content = Package <$> n <*> v <*> c <*> p where
      p' = Tar.entryPath entry
      p = p' <$ guard (".cabal" `isSuffixOf` p')
      c = pure content
      (n, v) = case splitDirectories p' of
            (name':versionStr:_) -> (Just name', readVersion versionStr)
            _ -> (Nothing, Nothing)



-- | Parse a version string a la "1.2.3".
readVersion :: String -> Maybe Version
readVersion = fmap Version . traverse readMaybe . splitOn "."



-- | Group packages by name. Assumes the unput is already sorted.
groupPackages :: [Package] -> [[Package]]
groupPackages = groupBy (\x y -> name x == name y)



-- | Find the package with the latest version.
latest :: [Package] -> Package
latest = maximumBy (comparing version)



-- | Searche the package DB for all dependencies of a package.
getDependencies :: Package
                -> Maybe [String] -- ^ Dependency package names
getDependencies = genericPackDescription >=> dependencies >=> extractNames

      where

      genericPackDescription :: Package -> Maybe Cabal.GenericPackageDescription
      genericPackDescription (Package { dotCabal = c }) =
            case Cabal.parsePackageDescription (EvilHack.unpack c) of
                  Cabal.ParseFailed _e  -> Nothing
                  Cabal.ParseOk _w deps -> Just deps

      packageDescription :: Cabal.GenericPackageDescription
                   -> Either [Cabal.Dependency]
                             (Cabal.PackageDescription, Cabal.FlagAssignment)
      packageDescription = Cabal.finalizePackageDescription
                           [] -- "flag assignments", whatever that may be
                           (const True)
                           Cabal.buildPlatform
                           (Cabal.CompilerId Cabal.buildCompilerFlavor
                                             (Cabal.Version [] []))
                           [] -- Additional constraints

      dependencies :: Cabal.GenericPackageDescription
                     -> Maybe [Cabal.Dependency]
      dependencies g = case packageDescription g of
            Right (descr, _) -> Just (Cabal.buildDepends descr)
            _ -> Nothing

      extractNames :: [Cabal.Dependency] -> Maybe [String]
      extractNames = Just . map getDepName



-- | Accessor to the name of a 'Cabal.Dependency'.
getDepName :: Cabal.Dependency -> String
getDepName (Cabal.Dependency depName _) = getPName depName



-- | Accessor to the name of a 'Cabal.PackageName'.
getPName :: Cabal.PackageName -> String
getPName (Cabal.PackageName pName) = pName



-- | Convert a Package to a pair of its own name and a list of dependencies
packageToNode :: Package -> Maybe (String, [String])
packageToNode p = (,) <$> pName <*> pDeps
      where pName = pure (name p)
            pDeps = fmap sort (getDependencies p)



-- | Read package database, generate graph
makeGraph :: FilePath -- ^ Path to package database, (00-index.tar)
          -> IO Graph
makeGraph packageDB = do
      tarDB <- BSL.readFile packageDB

      let allPackages :: [Package]
          allPackages = getPackages (Tar.read tarDB)

          latestPackages :: [Package]
          latestPackages = map latest (groupPackages allPackages)

          packAndDeps :: [(String, [String])]
          packAndDeps = mapMaybe packageToNode latestPackages

      return (Graph packAndDeps)