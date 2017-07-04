-- | Create a 'Graph' of a Hackage index.

module MakeGraph (makeGraph) where



import qualified Codec.Archive.Tar          as Tar
import           Control.Monad
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Data.Ord
import           System.FilePath            (splitDirectories)
import           Text.Printf
import           Text.Read

import           Data.Map  (Map)
import qualified Data.Map  as Map
import           Data.Set  (Set)
import qualified Data.Set  as Set
import           Data.Text (Text)
import qualified Data.Text as T

import qualified Distribution.Package                          as Cabal
import qualified Distribution.PackageDescription               as Cabal
import qualified Distribution.PackageDescription.Configuration as Cabal
import qualified Distribution.PackageDescription.Parse         as Cabal
import qualified Distribution.Simple.Compiler                  as Cabal
import qualified Distribution.System                           as Cabal

import Graph (Graph (..))



-- | A package, represented by a .cabal file.
data Package = Package
    { name     :: Text           -- ^ Package name
    , version  :: Version        -- ^ Package version
    , dotCabal :: BSL.ByteString -- ^ Content of .cabal
    , _path    :: FilePath       -- ^ Path to .cabal
    }

instance Show Package where
    show (Package n v _c p) = printf
        "Package %s v%s at %s"
        (T.unpack n)
        (show v)
        p

-- | a.b.c.d
newtype Version = Version [Int]
    deriving (Eq, Ord)

instance Show Version where
    show (Version vs) = intercalate "." (map show vs)

-- | 'String' to 'Text' conversion, stripped of enclosing whitespace.
packStripped :: String -> Text
packStripped = T.strip . T.pack

-- | Extract .tar file contents, and put them into a flat list
getPackages :: Show e
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
          (name':versionStr:_) -> ( Just (packStripped name')
                                  , readVersion versionStr
                                  )
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
                -> Maybe (Set Text) -- ^ Dependency package names
getDependencies = genericPackDescription >=> dependencies >=> extractNames
  where
    genericPackDescription :: Package -> Maybe Cabal.GenericPackageDescription
    genericPackDescription Package{ dotCabal = c } =
        case Cabal.parsePackageDescription (BSL8.unpack c) of
            Cabal.ParseFailed _e  -> Nothing
            Cabal.ParseOk _w deps -> Just deps

    packageDescription
        :: Cabal.GenericPackageDescription
        -> Either [Cabal.Dependency]
                  (Cabal.PackageDescription, Cabal.FlagAssignment)
    packageDescription = Cabal.finalizePackageDescription
        [] -- "flag assignments", whatever that may be
        (const True)
        Cabal.buildPlatform
        (Cabal.unknownCompilerInfo Cabal.buildCompilerId Cabal.NoAbiTag)
        [] -- Additional constraints

    dependencies :: Cabal.GenericPackageDescription
                 -> Maybe [Cabal.Dependency]
    dependencies g = case packageDescription g of
        Right (descr, _) -> Just (Cabal.buildDepends descr)
        _ -> Nothing

    extractNames :: [Cabal.Dependency] -> Maybe (Set Text)
    extractNames = Just . Set.fromList . map getDepName

-- | Accessor to the name of a 'Cabal.Dependency'.
getDepName :: Cabal.Dependency -> Text
getDepName (Cabal.Dependency depName _) = getPName depName
  where
    getPName :: Cabal.PackageName -> Text
    getPName (Cabal.PackageName pName) = packStripped pName

-- | Convert a Package to a pair of its own name and a list of dependencies
packageToNode :: Package -> Maybe (Text, Set Text)
packageToNode p = (,) <$> pName <*> pDeps
  where
    pName = pure (name p)
    pDeps = getDependencies p

-- | Read package database, generate graph
makeGraph :: FilePath -- ^ Path to package database, (00-index.tar)
          -> IO Graph
makeGraph packageDB = do
    tarDB <- BSL.readFile packageDB

    let allPackages :: [Package]
        allPackages = getPackages (Tar.read tarDB)

        latestPackages :: [Package]
        latestPackages = map latest (groupPackages allPackages)

        packAndDeps :: Map Text (Set Text)
        packAndDeps = Map.fromList (mapMaybe packageToNode latestPackages)

    pure (Graph packAndDeps)
