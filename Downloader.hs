{-
  To the extent possible under law, favonia (favonia@gmail.com)
  has waived all copyright and related or neighboring rights to this file.
-}

{-# LANGUAGE OverloadedStrings #-}

module Downloader where

import Prelude hiding (takeWhile)
import Control.Applicative
import Control.Monad
import System.IO
import System.IO.Error
import System.Process
import qualified Data.Text.IO as T
import Data.Text (Text)
import Data.Attoparsec.Text

skipTill :: Parser a -> Parser b -> Parser b
skipTill junk end = end <|> (junk *> skipTill junk end)

dataKeyword :: Text
dataKeyword = "downloadCheck.php?fn="

publicKeyword :: Text
publicKeyword = "download.php?fn="

readHtmlLinks :: Text -> FilePath -> IO (Either String [FilePath])
readHtmlLinks keyword path = parseOnly (many findLink) <$> T.readFile path
  where
    findLink :: Parser FilePath
    findLink = skipTill anyChar
             $ string "<a target=\"_blank\" href=\""
            *> string keyword
            *> many (notChar '"')

-- XXX: This can't deal with all HTTP standard-compliant responses.
skipLine :: Parser ()
skipLine = skipTill anyChar endOfLine

-- XXX: This can't deal with all HTTP standard-compliant responses.
findContentLength :: Parser Integer
findContentLength = skipTill skipLine parseContentLength
  where
    parseContentLength :: Parser Integer
    parseContentLength = string "Content-Length: " *> decimal <* endOfLine

-- XXX: This can't deal with all HTTP standard-compliant responses.
findEtag :: Parser Text
findEtag = skipTill skipLine parseEtag
  where
    parseEtag :: Parser Text
    parseEtag = string "ETag: " *> takeWhile (\c -> c /= '\r' && c /= '\n')

appendHeader :: FilePath -> FilePath
appendHeader = (++ ".header")

appendEtag :: FilePath -> FilePath
appendEtag = (++ ".etag")

storeEtagIfDone :: FilePath -> IO ()
storeEtagIfDone filename = do
  progress <- getSize
  header <- T.readFile $ appendHeader filename
  when (parseOnly findContentLength header /= Right progress) $ return ()
  case parseOnly findEtag header of
    Right etag -> T.writeFile (appendEtag filename) etag
    _ -> return ()
  where
    getSize :: IO Integer
    getSize = withFile filename ReadMode hFileSize

loadEtag :: FilePath -> IO (Maybe String)
loadEtag filename = (Just <$> readFile (appendEtag filename))
  `catchIOError` (\_ -> return Nothing)

curlDataFile :: FilePath -> IO ()
curlDataFile filename = do
  putStrLn $ "Downloading " ++ filename
  maybeEtag <- loadEtag filename
  etagArgs <- case maybeEtag of
    Just etag -> do
      putStrLn $ "Found Etag: " ++ etag
      return ["--header", "If-None-Match: " ++ etag]
    Nothing -> return []
  void $ rawSystem "curl" $
    ["-o", filename, "-R"] ++
    etagArgs ++
    ["--dump-header", appendHeader filename] ++
    ["-#"] ++
    ["http://www.ios.sinica.edu.tw/sc/cht/datafile/" ++ filename]
  storeEtagIfDone filename
  return ()
