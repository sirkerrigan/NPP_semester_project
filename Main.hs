{-# LANGUAGE NamedFieldPuns #-}

-- Main.hs
-- CLI: loads JSON, optimizes layout, prints summary and layout,
--      optionally writes HTML with optional CSS href.

module Main where

import System.Environment (getArgs)
import System.Exit        (exitFailure)
import qualified Data.Text as T

import Parser     (loadLayoutFile, prettySummary)
import Layout     (optimizeLayout, prettyLayout)
import HtmlExport (writeHtml, writeHtmlWithCss)

data Opts = Opts
  { inp  :: FilePath
  , out  :: Maybe FilePath
  , css  :: Maybe FilePath
  } deriving (Show)

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Left err -> do
      putStrLn err
      putStrLn "Usage:"
      putStrLn "  cabal run -- <layout.json>"
      putStrLn "  cabal run -- <layout.json> --out <out.html> [--css <style.css>]"
      exitFailure
    Right opts -> runOnce opts

runOnce :: Opts -> IO ()
runOnce Opts{inp, out, css} = do
  parsed <- loadLayoutFile inp
  case parsed of
    Left err -> putStrLn ("Parse error: " ++ err) >> exitFailure
    Right doc -> do
      putStrLn (prettySummary doc)
      putStrLn "---- Optimized Layout ----"
      let lay = optimizeLayout doc
      putStrLn (prettyLayout lay)
      case out of
        Nothing      -> pure ()
        Just outFile -> case css of
          Nothing     -> do
            writeHtml outFile doc lay
            putStrLn ("HTML written to: " ++ outFile)
          Just cssRef -> do
            writeHtmlWithCss outFile (T.pack cssRef) doc lay
            putStrLn ("HTML written to: " ++ outFile ++ " (css=" ++ cssRef ++ ")")

-- very tiny arg parser: <json> [--out FILE] [--css FILE]
parseArgs :: [String] -> Either String Opts
parseArgs (fp:rest) = go (Opts fp Nothing Nothing) rest
  where
    go :: Opts -> [String] -> Either String Opts
    go o []                 = Right o
    go o ("--out":f:xs)     = go o{out=Just f} xs
    go o ("--css":f:xs)     = go o{css=Just f} xs
    go _ xs                 = Left ("Unknown args: " ++ unwords xs)
parseArgs _ = Left "Missing <layout.json>"
