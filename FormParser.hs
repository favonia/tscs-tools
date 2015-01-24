{-
  To the extent possible under law, favonia (favonia@gmail.com)
  has waived all copyright and related or neighboring rights to this file.
-}

{-# LANGUAGE OverloadedStrings #-}

module FormParser (parseForm, testParser)  where

import Prelude hiding (takeWhile)
import Control.Applicative
import Control.Monad
import System.Process
import Data.Text (Text)
import qualified Data.Text as T
import Data.Attoparsec.Text
import Test.QuickCheck

counter :: Parser Text
counter = choice (map realize patterns)
  where
    realize :: (Parser Text -> Parser Text) -> Parser Text
    realize pattern = pattern $ T.pack <$> many1 (letter <|> digit)

    patterns :: [Parser Text -> Parser Text]
    patterns = [(<* string ". "), \p -> char '(' *> p <* char ')']

endOfInputOrLine :: Parser ()
endOfInputOrLine = endOfInput <|> endOfLine

endOfInputOrLineOrPage :: Parser ()
endOfInputOrLineOrPage =
  endOfInput <|> (endOfLine *> (turnPage <|> return ()))
  where
    pageBreak :: Parser Char
    pageBreak = char '\f'

    pageHeader :: Parser ()
    pageHeader = takeWhile (/= '\n') <* endOfInputOrLine *> many1 digit *> endOfInputOrLine

    turnPage :: Parser ()
    turnPage = pageBreak *> (pageHeader <|> endOfInput)

line :: Parser Text
line = takeWhile (/= '\n') <* endOfInputOrLineOrPage

hole :: Char
hole = '□'

skipHoles :: Parser ()
skipHoles = void $ takeWhile (\c -> c == ' ' || c == hole)

questionChar :: Parser Char
questionChar = satisfy (\c -> c /= '\n' && c /= hole)

questionSep :: Parser ()
questionSep = skipHoles *> endOfInputOrLineOrPage

skipTill :: Parser a -> Parser b -> Parser b
skipTill junk end = end <|> (junk *> skipTill junk end)

skipTillQuestions :: Parser [(Text, Text)]
skipTillQuestions = skipTill line questions

questions :: Parser [(Text, Text)]
questions =  (endOfInput *> return [])
         <|> (labelFst <$> counter <*> incompleteQuestionAndQuestions)
  where
    labelFst l (q, rest) = (l, q) : rest

incompleteQuestionAndQuestions :: Parser (Text, [(Text, Text)])
incompleteQuestionAndQuestions =
      (choice [options, topDownOptions, range, columns] *> (prependEmpty <$> skipTillQuestions))
  <|> (questionSep *> ((prependEmpty <$> questions) <|> incompleteQuestionAndQuestions))
  <|> (notes *> incompleteQuestionAndQuestions)
  <|> (consQuestion <$> questionChar <*> incompleteQuestionAndQuestions)
  where
    prependEmpty rest = ("", rest)

    consQuestion c (q, rest) = (T.cons c q, rest)

    options :: Parser ()
    options = char hole *> notChar hole *> skipWhile (/= '\n') *> endOfInputOrLineOrPage

    topDownOptions :: Parser ()
    topDownOptions = string "最頂層 " *> options

    range :: Parser ()
    range = skipWhile (==' ') *> many1 digit `sepBy1` char ' ' *> skipHoles *> endOfInputOrLineOrPage

    -- "同不同意無所謂" 其實是 "無所謂同不同意"
    columnWords :: [String]
    columnWords =
      [ "非常同意同意不同意非常不同意不一定"
      , "非常同意同意同不同意無所謂不同意非常不同意"
      , "非常同意同意同不同意無所謂不同意非常不同意無法選擇"
      , "做全職做兼職在家中工作沒有工作不適用"
      , "做全職做兼職不要外出工作的想法尊重當事人自己無法選擇"
      , "總是母親做經常是母親做或一起做父母親做的一樣多經常是父親做總是父親做大都是女性做大都是小孩做其他家人或其他人做無法選擇"
      , "總是我經常是我或一起做兩人做的一樣多(或同居伴侶)經常是我的配偶(或同居伴侶)總是我的配偶由其他的人來做無法選擇不適用"
      , "總有幾次一星期總有幾次一個月有過一兩次從來沒有"
      , "經常有時很少從來沒有"
      ]

    columns :: Parser ()
    columns = void $ choice (map eatUp columnWords)
      where
        eatUp :: String -> Parser ()
        eatUp str = forM_ str $ \c -> char c *> endOfInputOrLine

    notes :: Parser ()
    notes = void $  string "受訪者自己計算" *> skipWhile (==' ') *> string "訪員幫忙計算"
                <|> string "(訪員請唸出選項)"
                -- <|> skipWhile (==' ') *> string "（提示卡 " *> many1 digit *> string "）"

pages :: Parser [(Text, Text)]
pages = skipTillQuestions

parseForm :: FilePath -> IO [(Text, Text)]
parseForm path = do
  text <- readProcess "pdftotext" ["-raw", path, "-"] ""
  case parseOnly pages (T.pack text) of
    Left e -> fail e
    Right pairs -> return pairs

testParser :: IO ()
testParser =
  mapM_ quickCheck
    [ parseOnly counter "D1a. " == Right "D1a"
    , parseOnly counter "(a)" == Right "a"
    , parseOnly questions "1. X" == Right [("1", "X")]
    , parseOnly questions "X1. X□ □□" == Right [("X1", "X")]
    , parseOnly questions "F2. X □□ □□" == Right [("F2", "X")]
    , parseOnly questions "Z10a. X □Y □□\nZ10b. Z □□ □□\n" == Right [("Z10a", "X "), ("Z10b", "Z")]
    , parseOnly incompleteQuestionAndQuestions "\nA2. Y\n" == Right ("", [("A2", "Y")])
    , parseOnly questions "P1. X\nP2. Y\n" == Right [("P1", "X"), ("P2", "Y")]
    , parseOnly questions "a. A\nB\nC □□ □□\n\fXXX\n16\nb. D □\n" == Right [("a", "ABC"), ("b", "D")]
    ]
