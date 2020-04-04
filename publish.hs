{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.DateTime (getCurrentTime, formatDateTime)
import System.Environment (getArgs)
import System.FilePath    (takeBaseName)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder.Int as TB

import Text.Mustache (ToMustache(..), (~>), object, substitute, localAutomaticCompile)
import Text.Pandoc   (def, readMarkdown, writeHtml5String, runIOorExplode, readerExtensions, strictExtensions)

outFile = flip (++) ".html" . takeBaseName

toTitle = map (\c -> if c == '_' then ' ' else c) . takeBaseName

mdToHtml contents =
  runIOorExplode
    (readMarkdown def{readerExtensions=strictExtensions} contents >>= writeHtml5String def)

data Args = Args { post :: T.Text, title :: String, date :: String }

instance ToMustache Args where
  toMustache args = object
      [ "post" ~> post args
      , "title" ~> title args
      , "date" ~> date args
      ]

makeArgs name contents = do
  date <- getCurrentTime
  post <- mdToHtml contents
  return (Args { post=post
               , title=toTitle name
               , date=formatDateTime "%m/%d/%Y" date
               })

render target contents = do
  compiled <- localAutomaticCompile "layout.html"
  case compiled of
    Left err -> error (show err)
    Right template -> do
      args <- makeArgs target contents
      return (substitute template args)

main = do
  args <- getArgs
  if length args /= 1
  then error "Please provide a target to render."
  else do
    let target = head args
    contents <- T.readFile target
    rendered <- render target contents
    T.writeFile (outFile target) rendered
