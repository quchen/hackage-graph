{-# LANGUAGE LambdaCase #-}

-- | Create a 'Graph' of a Hackage index.
module PackageGraph (makeGraph) where



import qualified Codec.Archive.Tar          as Tar
import           Control.Monad
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import           Data.Map                   (Map)
import qualified Data.Map                   as M
import           Data.Set                   (Set)
import qualified Data.Set                   as S
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Pipes
import qualified Pipes.Prelude              as Pipes
import           System.FilePath

import qualified Distribution.Package                          as Cabal
import qualified Distribution.PackageDescription               as Cabal
import qualified Distribution.PackageDescription.Configuration as Cabal
import qualified Distribution.PackageDescription.Parse         as Cabal
import qualified Distribution.Simple.Compiler                  as Cabal
import qualified Distribution.System                           as Cabal
import qualified Distribution.Version                          as Cabal

import Graph



readTar :: FilePath -- ^ Path to package database, (00-index.tar)
        -> IO (Tar.Entries Tar.FormatError)
readTar packageDB = fmap Tar.read (BSL.readFile packageDB)

unTar :: Monad m => Tar.Entries Tar.FormatError -> Producer Tar.Entry m ()
unTar = \case
    Tar.Next x xs -> yield x >> unTar xs
    Tar.Done      -> pure ()
    Tar.Fail e    -> error ("tar failed: " ++ show e)

extractRawDotCabal :: Monad m => Pipe Tar.Entry BSL8.ByteString m a
extractRawDotCabal = forever (do
    tarEntry <- await
    let isDotCabal = (== ".cabal") . takeExtension . Tar.entryPath
    case (isDotCabal tarEntry, Tar.entryContent tarEntry) of
        (True, Tar.NormalFile content _size) -> yield content
        _otherwise -> pure () )

readPackage :: Monad m => Pipe BSL8.ByteString Cabal.PackageDescription m a
readPackage = forever (do
    rawDotCabal <- await
    case Cabal.parsePackageDescription (BSL8.unpack rawDotCabal) of
        Cabal.ParseFailed _err -> pure () -- Ignore broken packages
        Cabal.ParseOk _warnings pkgDescr ->
            let finalize = Cabal.finalizePackageDescription
                    [] -- flag assignments
                    (const True)
                    Cabal.buildPlatform
                    (Cabal.unknownCompilerInfo Cabal.buildCompilerId Cabal.NoAbiTag)
                    [] -- Additional constraints
            in case finalize pkgDescr of
                Left _missingDeps -> error "Bad deps"
                Right (pkg, _) -> yield pkg )

packageName :: Cabal.PackageDescription -> Text
packageName = T.pack . Cabal.unPackageName . Cabal.pkgName  . Cabal.package

packageVersion :: Cabal.PackageDescription -> [Int]
packageVersion = Cabal.versionBranch . Cabal.pkgVersion . Cabal.package

dependencies :: Cabal.PackageDescription -> Set Text
dependencies pkg
  = let depName (Cabal.Dependency (Cabal.PackageName x) _vrange) = T.pack x
    in (S.fromList . map depName . Cabal.buildDepends) pkg

aggregateNewest
    :: Monad m
    => Producer Cabal.PackageDescription m ()
    -> m (Map Text Cabal.PackageDescription)
aggregateNewest = Pipes.fold (flip insertNewest) M.empty id

insertNewest
    :: Cabal.PackageDescription
    -> Map Text Cabal.PackageDescription
    -> Map Text Cabal.PackageDescription
insertNewest thisPkg db = case M.lookup (packageName thisPkg) db of
    Nothing -> M.insert (packageName thisPkg) thisPkg db
    Just otherPkg
        | packageVersion thisPkg > packageVersion otherPkg -> M.insert (packageName thisPkg) thisPkg db
        | otherwise -> db

makeGraph :: FilePath -> IO (Graph Cabal.PackageDescription ())
makeGraph packageDB = do
    tar <- readTar packageDB
    pkgs <- aggregateNewest (unTar tar >-> extractRawDotCabal >-> readPackage)
    pure (Graph (M.fromList [ (Labeled pkgDescr pkgName, Labeled () (dependencies pkgDescr)) | (pkgName, pkgDescr) <- M.assocs pkgs ]))
