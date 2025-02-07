-- |
-- We expect to be called from the root of the stackage-snapshorts repo.
-- This repo has the following structure:
--
--   - @lts/MAJOR/MINOR.yaml@, e.g. @lts/21/1.yaml@
--
--   - @nightly/YEAR/MONTH/DAY.yaml@, e.g. @nightly/2023/7/3.yaml@
--
-- We first need to map each major GHC version to its latest snapshot.
-- To this end, we investigate the following candidates:
--
--    - @lt/MAJOR/MAXMINOR.yaml@ for each @MAJOR@ LTS version
--
--    - @nightly/MAXYEAR/MAXMONTH/MAXDAY.yaml@
--
-- The @.yaml@ files have the following fields of interest:
--
--   - compiler: ghc-MAJOR.MAJOR2.MINOR
--   - packages: [{ hackage: PACKAGE-VERSION@SHA, ...}]
--
-- Poor man's parsing could go through lines and pick the following:
--
--   - e.g. @compiler: ghc-7.8.4@ (one per file), may be indented
--   - e.g. @- hackage: Agda-2.6.3@sha256:7979e244ee659b9663305b5c545cff090f2309ed08d3e4ea3e7120579b8f047e,36742@ (many)
--
-- However, we can also use the (slower) proper YAML parser.
--

-- imports from standard libraries
import Control.Monad
  ( guard )
import Control.Monad.Except
  ( MonadError(..), ExceptT, runExceptT )
import Control.Monad.IO.Class
  ( MonadIO, liftIO )
import Data.Bifunctor
  ( second )
import Data.ByteString qualified as BS
import Data.Char
  ( isDigit )
import Data.Functor
  ( (<&>) )
import Data.List
  ( intercalate, sort, sortBy )
import Data.Map qualified as Map
import Data.Map
  ( Map )
import Data.Maybe
  ( catMaybes, listToMaybe, mapMaybe )
import Data.Set qualified as Set
import Data.Text    qualified as Text
import Data.Text
  ( Text )
import Data.Traversable
  ( forM )
import System.Directory
  ( doesDirectoryExist, listDirectory )
import System.Exit
  ( die )
import System.FilePath
  ( (</>), (<.>), dropExtension )
import Text.Read
  ( readMaybe )

-- extra
import Control.Monad.Extra
  ( fromMaybeM, unlessM )

-- yaml
import Data.Yaml
  ( (.:), (.:?), FromJSON(..), pattern Object, decodeThrow )

-- time
import Data.Time.Clock
  ( getCurrentTime )
import Data.Time.Format
  ( defaultTimeLocale, formatTime )

-- colour, default, lens
import Control.Lens
  ( (.~) )
import Data.Default
  ( def )
import Data.Colour
  ( AlphaColour, opaque )
import Data.Colour.SRGB
  ( sRGB24read )

-- Chart and Chart-cairo
import Graphics.Rendering.Chart
  ( Layout,
    PlotIndex,
    addIndexes,
    autoIndexAxis,
    laxis_generate,
    layoutToRenderable,
    layout_bottom_axis_visibility,
    layout_legend,
    layout_plots,
    layout_title,
    layout_x_axis,
    legend_orientation,
    plotBars,
    plot_bars_item_styles,
    plot_bars_spacing,
    plot_bars_style,
    plot_bars_titles,
    plot_bars_values,
    pattern AxisVisibility,
    pattern BarsFixGap,
    pattern BarsStacked,
    pattern FillStyleSolid,
    pattern LORows,
  )
import Graphics.Rendering.Chart.Backend.Cairo
  ( pattern FileOptions, pattern PDF, pattern SVG
  , renderableToFile
  )

-- * Configuration

-- | Path to LTS snapshots from repository root.
ltsPath :: FilePath
ltsPath = "lts"

-- | Path to nightly snapshots from repository root.
nightlyPath :: FilePath
nightlyPath = "nightly"

-- | Size of generated images.
dim :: (Int, Int)
dim = (850, 600)

-- | Palette generated via <https://gka.github.io/palettes/#/15|d|488f31,ffe692|ffe692,de425b|1|1>
colors :: [AlphaColour Double]
colors = map opaque $ cycle $ map sRGB24read $ reverse $
  ["#488f31", "#649c3f", "#7fa84c", "#98b55a", "#b2c167", "#cbcd75", "#e5da84", "#ffe692", "#fcd18a", "#f9bb82", "#f5a57a", "#f08f72", "#eb786a", "#e55f63", "#de425b"]

-- * Types

-- ** Snapshot files

-- | Fragment of the grammar of stackage snapshots.
data Snapshot = Snapshot
  { packages :: [SnapshotPackage]
      -- ^ The list of versioned packages included in the snapshot.
  , compiler :: Maybe Text
      -- ^ Info about the compiler version for this snapshot (early format).
  , resolver :: Maybe SnapshotResolver
      -- ^ Info about the compiler version for this snapshot (late format).
  } deriving Show

instance Eq  Snapshot where _ == _      = undefined
instance Ord Snapshot where compare _ _ = undefined

-- | Fragment of the @"packages"@ entries of a stackage snapshot.
data SnapshotPackage = SnapshotPackage
  { hackage :: Text
      -- ^ The hackage identifier (plus SHA) of a package.
  } deriving Show

-- | The @"resolver"@ entry of a stackage snapshot.
data SnapshotResolver = SnapshotResolver
  { compiler :: Text
      -- ^ The Haskell compiler for this snapshot, e.g. @ghc-9.6.2@.
  } deriving Show

-- ** Extracted information from snapshots

-- | A LTS version.
data LTS = LTS { major, minor :: Int }
  deriving (Eq, Ord, Show)

-- | A nightly version.
data Nightly = Nightly { year, month, day :: Int }
  deriving (Eq, Ord, Show)

-- | A GHC major version.
data GHC = GHC { major, minor :: Int }
  deriving (Eq, Ord, Show)

-- | Name of a Haskell package.
newtype Package = Package { packageName :: Text }
  deriving (Eq, Ord, Show)

-- ** Monad

-- | Our monad.
newtype M a = M { unM :: ExceptT Err IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError Err)

data Err
  = NoNightly
      -- ^ Directory @nightly@ not found at the root.
  | NoNightlyYear
      -- ^ No directory @nightly/YEAR@ found at the root.
  | NoNightlyMonth
      -- ^ No directory @nightly/MAXYEAR/MONTH@ found at the root.
  | NoNightlyDay
      -- ^ No file @nightly/MAXYEAR/MAXMONTH/DAY.EXTENSION@ found at the root.
  | NoLTS
      -- ^ Directory @lts@ not found at the root.
  | NoLTSMinor Int
      -- ^ Directory @lts/Int@ has no files @MINOR.EXTENSION@.
  deriving Show

runM :: M a -> IO a
runM m = runExceptT (unM m) >>= \case
  Left err -> die $ show err
  Right a  -> return a

-- * Code

main :: IO ()
main = do
  putStrLn "stackage-history-chart (C) Andreas Abel 2023"

  putStrLn "Parsing latest snapshots"
  -- Get LTS snapshots (latest major versions).
  ltss <- runM latestLTSs
  -- mapM_ putStrLn ltss
  ghcLTSs :: [(GHC,(LTS,Snapshot))]
    -- We ignore the snapshots that have no "compiler: ghc-..." entry.
    <- catMaybes <$>
      forM ltss \ lts -> do
        s :: Snapshot
          <- decodeThrow =<< BS.readFile (ltsToFilePath lts)
        reportSnapshot (ltsToID lts) s
        pure $ snapshotGHC s <&> (,(lts,s))

  -- Get latest nightly snapshot.
  nightly <- runM latestNightly
  -- putStrLn latest
  nightlySnapshot :: Snapshot
    <- decodeThrow =<< BS.readFile (nightlyToFilePath nightly)
  -- mapM_ (Text.putStrLn . packageName) $ snapshotPackages nightlySnapshot
  reportSnapshot (nightlyToID nightly) nightlySnapshot

  -- For each major GHC version, get the latest snapshot.
  -- Nightly overwrites a potential LTS snapshot with the same GHC major version.
  let
    ghcToSnapshot :: Map GHC Snapshot
    ghcToSnapshot =
      -- Nightly
      maybe id (\ ghc -> Map.insert ghc nightlySnapshot) (snapshotGHC nightlySnapshot)
      -- LTSs
      $ fmap snd $ Map.fromListWith max ghcLTSs

    ghcToPackages :: [(GHC, [Package])]
    ghcToPackages = Map.toList $ fmap snapshotPackages ghcToSnapshot

  -- Map each package to its minium GHC version.
  let
    pkgToMin :: Map Package GHC
    pkgToMin =
      Map.map minimum $
      Map.fromListWith (<>) $
      concatMap (\ (ghc, pkgs) -> map (,Set.singleton ghc) pkgs) $
      ghcToPackages

  -- Map each GHC version to number packages per smaller-or-equal GHC version.
  let
    pkgMaps :: [(GHC, [Int])]
    pkgMaps = flip map ghcToPackages $ second $
      Map.elems .
      Map.fromListWith (+) .
      map (\ pkg -> (pkgToMin Map.! pkg, 1))

  -- Prepare a chart visualizing pkgMaps.

  today :: String
    <- formatTime defaultTimeLocale "%F" <$> getCurrentTime

  let
    ghcs :: [String]
    ghcs = map (printGHC . fst) pkgMaps

    vals :: [[Int]]
    vals = map snd pkgMaps

    nBars = length ghcs

    bars = plot_bars_titles      .~ ghcs
         $ plot_bars_style       .~ BarsStacked
         $ plot_bars_spacing     .~ BarsFixGap 0 0
         $ plot_bars_item_styles .~ map (\ c -> (FillStyleSolid c, Just def)) colors
         $ plot_bars_values      .~ addIndexes vals
         $ def

    layout :: Layout PlotIndex Int
    layout = layout_title  .~ ("Stackage LTS packages by age in terms of GHC version as of " ++ today)
           $ layout_legend .~ Just (legend_orientation .~ LORows nBars $ def)
           $ layout_x_axis . laxis_generate .~ autoIndexAxis ghcs
           $ layout_plots  .~ [ plotBars bars ]
           $ layout_bottom_axis_visibility  .~ AxisVisibility True False True  -- only line, no numbers
           $ def

  let
    renderable = layoutToRenderable layout
    writeImage typ name = do
      putStrLn $ unwords [ "Writing", name ]
      renderableToFile (FileOptions dim typ) name renderable
    svg = ("chart-" ++ today ++ ".svg")
    pdf = ("chart-" ++ today ++ ".pdf")

  _ <- writeImage SVG svg
  _ <- writeImage PDF pdf
  return ()

-- | Print some basic statistics about a snapshot.
reportSnapshot :: String -> Snapshot -> IO ()
reportSnapshot name snapshot = do
  putStrLn $ unwords $ concat
    [ [ "-", name ]
    , maybe [] (\ ghc -> [ "for GHC", printGHC ghc ]) $ snapshotGHC snapshot
    , [ "has", show $ length $ snapshotPackages snapshot, "packages" ]
    ]

snapshotGHC :: Snapshot -> Maybe GHC
snapshotGHC s = listToMaybe $ mapMaybe (parseGHC . Text.unpack) $ catMaybes [ s.compiler, (.compiler) <$> s.resolver ]

parseGHC :: String -> Maybe GHC
parseGHC s = do
  let (ghc, ver) = splitAt 4 s
  guard $ ghc == "ghc-"
  case break ('.' ==) ver of
    (hd, '.':tl) -> do
      maj <- readMaybe hd
      min <- readMaybe $ takeWhile isDigit tl
      return $ GHC maj min
    _ -> Nothing

printGHC :: GHC -> String
printGHC (GHC major minor) = show major <> "." <> show minor

snapshotPackages :: Snapshot -> [Package]
snapshotPackages = map (parsePackageID . hackage) . packages

parsePackageID :: Text -> Package
parsePackageID = Package . Text.init . Text.dropWhileEnd (/= '-') . Text.takeWhile (/= '@')

ltsToID :: LTS -> String
ltsToID (LTS major minor) = "lts-" <> show major <> "." <> show minor

ltsToFilePath :: LTS -> FilePath
ltsToFilePath (LTS major minor) =
  ltsPath </> show major </> show minor <.> "yaml"

nightlyToID :: Nightly -> String
nightlyToID (Nightly year month day) = intercalate "-" $ "nightly" : map show [year, month, day]

nightlyToFilePath :: Nightly -> FilePath
nightlyToFilePath (Nightly year month day) =
  nightlyPath </> show year </> show month </> show day <.> "yaml"


-- | Get the stem to the latest nightly, e.g. @nightly/2023/7/3@.
latestNightly :: M Nightly
latestNightly = do
  -- @nightly@
  unlessM (liftIO $ doesDirectoryExist nightlyPath) $ throwError NoNightly
  -- @nightly/2023@
  year <- fromMaybeM (throwError NoNightlyYear) $ maxNumericEntry nightlyPath
  let nightlyYear = nightlyPath </> show year
  -- @nightly/2023/7@
  month <- fromMaybeM (throwError NoNightlyMonth) $ maxNumericEntry nightlyYear
  let nightlyYearMonth = nightlyYear </> show month
  -- @nightly/2023/7/3@
  day <- fromMaybeM (throwError NoNightlyDay) $ maxNumericEntry nightlyYearMonth
  return $ Nightly year month day

latestLTSs :: M [LTS]
latestLTSs = do
  -- @lts@
  unlessM (liftIO $ doesDirectoryExist ltsPath) $ throwError NoLTS
  -- @lts/MAJOR@
  majors <- sort . mapMaybe readMaybe <$> liftIO (listDirectory ltsPath)
  forM majors \ major -> do
    -- @lts/21@
    let ltsMajor = ltsPath </> show major
    -- @lts/21/1@
    minor <- fromMaybeM (throwError $ NoLTSMinor major) $ maxNumericEntry ltsMajor
    return $ LTS major minor

-- * Utilities

-- | Given a directory, select the entries that are numeric after dropping the extension,
--   and return the maximal one.
maxNumericEntry :: MonadIO io => FilePath -> io (Maybe Int)
maxNumericEntry root =
  listToMaybe . sortBy (flip compare) . mapMaybe readMaybe . map dropExtension <$> do
    liftIO $ listDirectory root

-- YAML parsing

instance FromJSON Snapshot where
  parseJSON (Object v) = Snapshot
    <$> v .:  "packages"
    <*> v .:? "compiler"
    <*> v .:? "resolver"

instance FromJSON SnapshotPackage where
  parseJSON (Object v) = SnapshotPackage
    <$> v.: "hackage"

instance FromJSON SnapshotResolver where
  parseJSON (Object v) = SnapshotResolver
    <$> v.: "compiler"
