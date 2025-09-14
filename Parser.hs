{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
-- Parser.hs
-- JSON loader for the layout optimizer: reads Config, Constraints, Components
-- and provides a tiny pretty-printer for quick CLI inspection.

module Parser
  ( Kind(..)
  , Constraint(..)
  , Component(..)
  , Config(..)
  , InputDoc(..)
  , loadLayoutFile
  , decodeLayout
  , prettySummary
  ) where

import           Data.Aeson
import           Data.Aeson.Types          (Parser)
import qualified Data.ByteString.Lazy      as BL
import           Data.List                 (intercalate)
import           Data.Maybe                (fromMaybe)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           GHC.Generics              (Generic)

-- | Semantic kinds supported by the generator/optimizer.
--   Keep the textual representation in sync with JSON (camelCase).

data Kind
  = TextBlock
  | Image
  | Header
  | Footer
  | Nav
  | Card
  deriving (Eq, Ord, Show, Generic)

instance FromJSON Kind where
  parseJSON = withText "Kind" $ \t -> case T.toLower t of
    "textblock" -> pure TextBlock
    "image"     -> pure Image
    "header"    -> pure Header
    "footer"    -> pure Footer
    "nav"       -> pure Nav
    "card"      -> pure Card
    other        -> fail ("Unknown kind: " ++ T.unpack other)

instance ToJSON Kind where
  toJSON k = String $ case k of
    TextBlock -> "textBlock"
    Image     -> "image"
    Header    -> "header"
    Footer    -> "footer"
    Nav       -> "nav"
    Card      -> "card"

-- | Global constraints requested by the instructor.

data Constraint
  = ImagesNeedTextNeighbor
  | FooterAtBottom
  | HeaderAtTop
  deriving (Eq, Ord, Show, Generic)

instance FromJSON Constraint where
  parseJSON = withText "Constraint" $ \t -> case t of
    "ImagesNeedTextNeighbor" -> pure ImagesNeedTextNeighbor
    "FooterAtBottom"         -> pure FooterAtBottom
    "HeaderAtTop"            -> pure HeaderAtTop
    other                     -> fail ("Unknown constraint: " ++ T.unpack other)

instance ToJSON Constraint where
  toJSON c = String $ case c of
    ImagesNeedTextNeighbor -> "ImagesNeedTextNeighbor"
    FooterAtBottom         -> "FooterAtBottom"
    HeaderAtTop            -> "HeaderAtTop"

-- | User-declared component to place on a grid.
--   Optional fields 'content' and 'src' let us render HTML later.

data Component = Component
  { compId          :: Text
  , kind            :: Kind
  , minSpan         :: Int
  , prefSpan        :: Int
  , maxSpan         :: Int
  , prefHeight      :: Int
  , lockRow         :: Bool
  , adjacentToText  :: Bool
  , group           :: Maybe Text
  , content         :: Maybe Text    -- ^ for textBlock/header/footer/card textual content
  , src             :: Maybe Text    -- ^ for image source path/URL
  } deriving (Eq, Show, Generic)

instance FromJSON Component where
  parseJSON = withObject "Component" $ \o -> do
    compId          <- o .:  "id"
    kind            <- o .:  "kind"
    minSpan         <- o .:  "minSpan"
    prefSpan        <- o .:  "prefSpan"
    maxSpan         <- o .:  "maxSpan"
    prefHeight      <- o .:  "prefHeight"
    lockRow         <- o .:  "lockRow"
    adjacentToText  <- o .:  "adjacentToText"
    group           <- o .:? "group"
    content         <- o .:? "content"
    src             <- o .:? "src"
    -- simple sanity checks
    if minSpan <= 0 || prefSpan <= 0 || maxSpan <= 0
      then fail "spans must be positive"
      else if not (minSpan <= prefSpan && prefSpan <= maxSpan)
        then fail "require minSpan <= prefSpan <= maxSpan"
        else pure Component{..}

instance ToJSON Component where
  toJSON Component{..} = object
    [ "id"              .= compId
    , "kind"            .= kind
    , "minSpan"         .= minSpan
    , "prefSpan"        .= prefSpan
    , "maxSpan"         .= maxSpan
    , "prefHeight"      .= prefHeight
    , "lockRow"         .= lockRow
    , "adjacentToText"  .= adjacentToText
    , "group"           .= group
    , "content"         .= content
    , "src"             .= src
    ]

-- | Optimizer configuration knobs.

data Config = Config
  { gridCols         :: Int
  , balancePenalty   :: Int
  , raggedPenalty    :: Int
  , hardMaxRowHeight :: Int
  , useBacktracking  :: Bool
  } deriving (Eq, Show, Generic)

instance FromJSON Config where
  parseJSON = withObject "Config" $ \o -> do
    gridCols         <- o .:  "gridCols"
    balancePenalty   <- o .:  "balancePenalty"
    raggedPenalty    <- o .:  "raggedPenalty"
    hardMaxRowHeight <- o .:  "hardMaxRowHeight"
    useBacktracking  <- o .:  "useBacktracking"
    if gridCols <= 0 then fail "gridCols must be > 0" else pure Config{..}

instance ToJSON Config where
  toJSON Config{..} = object
    [ "gridCols"         .= gridCols
    , "balancePenalty"   .= balancePenalty
    , "raggedPenalty"    .= raggedPenalty
    , "hardMaxRowHeight" .= hardMaxRowHeight
    , "useBacktracking"  .= useBacktracking
    ]

-- | Top-level document exactly matching the JSON file.

data InputDoc = InputDoc
  { config      :: Config
  , constraints :: [Constraint]
  , components  :: [Component]
  } deriving (Eq, Show, Generic)

instance FromJSON InputDoc where
  parseJSON = withObject "InputDoc" $ \o -> do
    config      <- o .:  "config"
    constraints <- o .:? "constraints" .!= []
    components  <- o .:  "components"
    pure InputDoc{..}

instance ToJSON InputDoc where
  toJSON InputDoc{..} = object
    [ "config"      .= config
    , "constraints" .= constraints
    , "components"  .= components
    ]

-- | Decode from a lazy ByteString

decodeLayout :: BL.ByteString -> Either String InputDoc
decodeLayout = eitherDecode'

-- | Load from file

loadLayoutFile :: FilePath -> IO (Either String InputDoc)
loadLayoutFile fp = decodeLayout <$> BL.readFile fp

-- | A concise multi-line summary for quick inspection in CLI logs.

prettySummary :: InputDoc -> String
prettySummary InputDoc{config=Config{..}, constraints, components} =
  let kinds = map (show . kind) components
      counts = countBy kinds
      countsStr = intercalate ", " [k ++ ": " ++ show n | (k,n) <- counts]
  in unlines
     [ "gridCols=" ++ show gridCols
     , "components=" ++ show (length components) ++ " (" ++ countsStr ++ ")"
     , "constraints=" ++ (if null constraints then "none" else intercalate ", " (map show constraints))
     ]
  where
    countBy :: [String] -> [(String, Int)]
    countBy = foldr bump []
    bump x [] = [(x,1)]
    bump x ((k,n):xs) | x==k      = (k,n+1):xs
                      | otherwise = (k,n):bump x xs
