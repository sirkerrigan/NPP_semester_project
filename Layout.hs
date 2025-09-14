{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module Layout
  ( Placement(..)
  , Row(..)
  , Layout(..)
  , optimizeLayout
  , prettyLayout
  ) where

import           Data.List       (minimumBy, intercalate)
import           Data.Ord        (comparing)
import           Data.Text       (Text)
import qualified Data.Text       as T

import           Parser          (Component(..), Kind(..), Constraint(..), Config(..), InputDoc(..))

-- | Final per-component placement in a grid.
--   'colSpan' is the number of columns occupied. 'x' is the starting column (1-based).
--   'rowH' is the height of the row this component participates in.
data Placement = Placement
  { pComp   :: Component
  , x       :: Int      -- ^ starting column (1..gridCols)
  , colSpan :: Int
  , rowH    :: Int
  } deriving (Eq, Show)

newtype Row    = Row    { cells :: [Placement] } deriving (Eq, Show)
newtype Layout = Layout { rows  :: [Row] }       deriving (Eq, Show)

-- | Pretty-print layout in a compact textual form for CLI inspection.
prettyLayout :: Layout -> String
prettyLayout (Layout rs) = unlines (zipWith pp [1..] rs)
  where
    pp i (Row ps) =
      let inside = intercalate " | "
            [ T.unpack (compId (pComp p))
              ++ "@" ++ show (x p)
              ++ "x" ++ show (colSpan p)
              ++ "[h=" ++ show (rowH p) ++ "]"
            | p <- ps
            ]
      in "Row " ++ show i ++ ": " ++ inside

-- | Optimize layout: generate candidates by backtracking and pick the best by score.
--   Header/Footer hard-constraints are filtered if possible.
optimizeLayout :: InputDoc -> Layout
optimizeLayout InputDoc{config=cfg@Config{..}, constraints, components} =
  let candidates = backtrack cfg constraints [] components
      hardOK l = and
        [ not (HeaderAtTop    `elem` constraints) || headerIsFirst (rows l)
        , not (FooterAtBottom `elem` constraints) || footerIsLast  (rows l)
        ]
      feas = filter hardOK candidates
      pool = if null feas then candidates else feas
  in case pool of
       []     -> Layout []
       (l:ls) -> minimumBy (comparing (score cfg)) (l:ls)

-- | Backtracking: build rows from left to right; start new row when needed.
backtrack :: Config -> [Constraint] -> [Row] -> [Component] -> [Layout]
backtrack _   _  acc []       = [Layout (reverse acc)]
backtrack cfg@Config{..} cs acc (c:rest)
  | lockRow c = placeFreshRow cfg cs acc c rest
  | otherwise =
      let tryNew  = placeFreshRow cfg cs acc c rest
          tryHere = case acc of
                      []      -> tryNew
                      (r:rs0) -> placeInRow cfg cs (r:rs0) c rest
      in tryHere ++ tryNew

-- | Place into a fresh row (and pull immediate group mates).
placeFreshRow :: Config -> [Constraint] -> [Row] -> Component -> [Component] -> [Layout]
placeFreshRow cfg@Config{..} cs acc c rest =
  let (groupMates, rest') = takeGroup (group c) rest
      rowComps            = c : groupMates
  in do
    row <- layoutRow cfg cs rowComps
    backtrack cfg cs (row:acc) rest'

-- | Try to append into the current last row.
placeInRow :: Config -> [Constraint] -> [Row] -> Component -> [Component] -> [Layout]
placeInRow cfg@Config{..} cs (r:rs0) c rest =
  let (groupMates, rest') = takeGroup (group c) rest
      rowComps            = c : groupMates
  in do
    row <- augmentRow cfg cs r rowComps
    backtrack cfg cs (row:rs0) rest'

-- | Extract consecutive components from the same group.
takeGroup :: Maybe Text -> [Component] -> ([Component], [Component])
takeGroup Nothing  xs = ([], xs)
takeGroup (Just g) xs = spanSameGroup xs
  where
    spanSameGroup (y:ys) | group y == Just g && not (lockRow y) =
      let (zs, rest) = spanSameGroup ys in (y:zs, rest)
    spanSameGroup ys = ([], ys)

-- | Layout a NEW row, respecting grid width and constraints.
layoutRow :: Config -> [Constraint] -> [Component] -> [Row]
layoutRow cfg@Config{..} cs csIn = do
  spans <- chooseSpans cfg csIn        -- one combination of spans
  let xs      = placeLinearly spans
      ph      = rowHeight csIn
      tooTall = hardMaxRowHeight > 0 && ph > hardMaxRowHeight
      ps      = [ Placement c x s ph | (c,(x,s)) <- zip csIn xs ]
  if tooTall then [] else if feasibleRow cs ps then [Row ps] else []

-- | Append to an existing row if it fits.
augmentRow :: Config -> [Constraint] -> Row -> [Component] -> [Row]
augmentRow cfg@Config{..} cs (Row ps0) csIn = do
  let used = sum (map colSpan ps0)
      wrem = gridCols - used
  spans <- chooseSpans cfg csIn
  let need = sum spans
  if need > wrem then [] else do
    let start  = 1 + sum (map colSpan ps0)
        xs     = placeLinearlyFrom start spans
        phNew  = max (rowHeight (map pComp ps0)) (rowHeight csIn)
        tooTall= hardMaxRowHeight > 0 && phNew > hardMaxRowHeight
        ps     = ps0 ++ [ Placement c x s phNew | (c,(x,s)) <- zip csIn xs ]
    if tooTall then [] else if feasibleRow cs ps then [Row ps] else []

-- | For each component produce span choices around prefSpan.
chooseSpans :: Config -> [Component] -> [[Int]]
chooseSpans _ cs = sequence [choices c | c <- cs]
  where
    choices Component{..} =
      let base  = [prefSpan]
          ups   = [prefSpan+1 .. maxSpan]
          downs = [prefSpan-1, prefSpan-2 .. minSpan]
      in base ++ ups ++ downs

placeLinearly :: [Int] -> [(Int,Int)]
placeLinearly spans = snd $ foldl step (1,[]) spans
  where
    step (pos,acc) s = (pos+s, acc ++ [(pos,s)])

placeLinearlyFrom :: Int -> [Int] -> [(Int,Int)]
placeLinearlyFrom start spans = snd $ foldl step (start,[]) spans
  where
    step (pos,acc) s = (pos+s, acc ++ [(pos,s)])

rowHeight :: [Component] -> Int
rowHeight = maximum . map prefHeight


feasibleRow :: [Constraint] -> [Placement] -> Bool
feasibleRow cs ps = and
  [ imagesHaveTextNeighbor
  , all (not . needsTextWithoutNeighbor) ps
  ]
  where
    -- текстовые виды
    isTextKind k = case k of
      TextBlock -> True
      Header    -> True
      Footer    -> True
      _         -> False

    neighbors p = [ q | q <- ps, q /= p, touchingOrOverlapping p q ]
    touchingOrOverlapping p q =
      let pStart = x p
          pEnd   = x p + colSpan p - 1
          qStart = x q
          qEnd   = x q + colSpan q - 1
          overlap = not (pEnd < qStart || qEnd < pStart)
          touch   = pEnd + 1 == qStart || qEnd + 1 == pStart
      in overlap || touch

    
    imagesNeedingText =
      [ p | p <- ps
          , kind (pComp p) == Image
          , adjacentToText (pComp p) == True
      ]
    imagesHaveTextNeighbor =
      all (\p -> any (isTextKind . kind . pComp) (neighbors p)) imagesNeedingText

    needsTextWithoutNeighbor p =
      let requires = adjacentToText (pComp p)
          hasTextN = any (isTextKind . kind . pComp) (neighbors p)
      in requires && not hasTextN

-- | Global scoring: prefer compact rows and balanced heights. Lower is better.
score :: Config -> Layout -> Int
score Config{..} (Layout rs) = ragged + balance
  where
    widths  = [ sum (map colSpan (cells r)) | r <- rs ]
    heights = [ case cells r of { [] -> 0; ps -> maximum (map rowH ps) } | r <- rs ]
    ragged  = raggedPenalty * sum [ gridCols - w | w <- widths ]
    balance = case heights of
                [] -> 0
                _  -> balancePenalty * (maximum heights - minimum heights)

-- | Helpers for header/footer checks.
footerIsLast :: [Row] -> Bool
footerIsLast [] = True
footerIsLast rs = any ((== Footer) . kind . pComp) (cells (last rs))

headerIsFirst :: [Row] -> Bool
headerIsFirst []    = True
headerIsFirst (r:_) = any ((== Header) . kind . pComp) (cells r)
