{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module HtmlExport
  ( renderHtml
  , renderHtmlWithCss
  , writeHtml
  , writeHtmlWithCss
  ) where

import qualified Data.Text    as T
import           Data.Text    (Text)
import qualified Data.Text.IO as T

import           Parser (Component(..), Kind(..), InputDoc(..), Config(..))
import           Layout (Layout(..), Row(..), Placement(..))

-- | full html page. css attaches byt a file style.css
renderHtml :: InputDoc -> Layout -> Text
renderHtml = renderHtmlWithCss "style.css"

-- | same thing, but can define path for css
renderHtmlWithCss :: Text -> InputDoc -> Layout -> Text
renderHtmlWithCss cssHref InputDoc{config=Config{..}} (Layout rs) =
  T.unlines $ header <> ["<body>"] <> rowsHtml <> ["</body>", "</html>"]
  where
    header =
      [ "<!DOCTYPE html>"
      , "<html lang=\"en\">"
      , "<head>"
      , "  <meta charset=\"utf-8\"/>"
      , "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\"/>"
      , "  <title>Layout</title>"
      , "  <link rel=\"stylesheet\" href=\"" <> cssHref <> "\"/>"
      , "</head>"
      ]

    rowsHtml :: [Text]
    rowsHtml = concatMap rowHtml rs

    rowHtml :: Row -> [Text]
    rowHtml (Row ps) =
      ["<section class=\"row\">"]
      <> concatMap placementHtml ps
      <> ["</section>"]

    placementHtml :: Placement -> [Text]
    placementHtml Placement{..} =
      let Component{..} = pComp
          spanClass = T.pack ("span-" ++ show colSpan)
          kindClass = T.toLower (T.pack (show kind))
          cellOpen  = T.unwords
            ["<div class=\"cell", spanClass, kindClass
            , "\" style=\"grid-column: span ", T.pack (show colSpan), ";\">"
            ]
          cellClose = "</div>"
      in case kind of
           Image ->
             let srcAttr = maybe "" id src
                 altTxt  = compId
             in [ cellOpen
                , "  <img src=\"" <> srcAttr <> "\" alt=\"" <> altTxt <> "\"/>"
                , cellClose
                ]
           Header    -> [ cellOpen, "  <header>" <> fromMaybeEmpty content <> "</header>", cellClose ]
           Footer    -> [ cellOpen, "  <footer>" <> fromMaybeEmpty content <> "</footer>", cellClose ]
           TextBlock -> [ cellOpen, "  <p>"      <> fromMaybeEmpty content <> "</p>",      cellClose ]
           Card      -> [ cellOpen, "  <div class=\"card\">" <> fromMaybeEmpty content <> "</div>", cellClose ]
           Nav       -> [ cellOpen, "  <nav>"    <> fromMaybeEmpty content <> "</nav>",    cellClose ]

    fromMaybeEmpty :: Maybe Text -> Text
    fromMaybeEmpty = maybe "" id

-- (is not used, but can leave it here)
baseCss :: Int -> Text
baseCss cols = T.unlines
  [ ":root{ --gap:16px; }"
  , "*{ box-sizing:border-box; }"
  , "body{ margin:0; font-family:system-ui, sans-serif; padding:var(--gap); background:#fafafa; }"
  , ".row{ display:grid; grid-template-columns: repeat(" <> tshow cols <> ", 1fr); gap: var(--gap); margin-bottom: var(--gap); }"
  , ".cell{ background:white; border:1px solid #e3e3e3; padding:12px; border-radius:10px; box-shadow:0 1px 2px rgba(0,0,0,.05); }"
  , "header, footer, nav{ font-weight:600; }"
  , "img{ max-width:100%; height:auto; display:block; border-radius:8px; }"
  , ".card{ padding:8px; }"
  ]
  where
    tshow = T.pack . show

-- | write html to a ffile
writeHtml :: FilePath -> InputDoc -> Layout -> IO ()
writeHtml fp doc lay = T.writeFile fp (renderHtml doc lay)

-- | write html with exact path to css
writeHtmlWithCss :: FilePath -> Text -> InputDoc -> Layout -> IO ()
writeHtmlWithCss fp cssHref doc lay = T.writeFile fp (renderHtmlWithCss cssHref doc lay)
