{-
  To the extent possible under law, favonia (favonia@gmail.com)
  has waived all copyright and related or neighboring rights to this file.
-}

{-# LANGUAGE OverloadedStrings #-}

module FormParser (parseForm, testParser)  where

import Prelude hiding (takeWhile)
import Control.Applicative
import Control.Monad
import Data.Char
import Data.List (intersperse)
import System.Process
import Data.Text (Text)
import qualified Data.Text as T
import Data.Attoparsec.Text
import Test.QuickCheck

skipWhile1 :: (Char -> Bool) -> Parser ()
skipWhile1 p = skip p *> skipWhile p

sepBy2 :: Parser a -> Parser b -> Parser [a]
sepBy2 p s = (:) <$> p <*> (s *> sepBy1 p s)

skipTill :: Parser a -> Parser b -> Parser b
skipTill junk end = end <|> (junk *> skipTill junk end)

skipChar :: Char -> Parser ()
skipChar = skip . (==)

isAsciiAlphaNum :: Char -> Bool
isAsciiAlphaNum c = isAscii c && isAlphaNum c

counter :: Parser Text
counter = choice (map realize patterns)
  where
    realize :: (Parser Text -> Parser Text) -> Parser Text
    realize pattern = pattern $ takeWhile1 isAsciiAlphaNum

    patterns :: [Parser Text -> Parser Text]
    patterns =
      [ (<* string ". ")
      , (<* string ".")
      , \p -> char '(' *> p <* char ')'
      ]

endOfInputOrLine :: Parser ()
endOfInputOrLine = endOfInput <|> endOfLine

endOfInputOrLineOrPage :: Parser ()
endOfInputOrLineOrPage =
  endOfInput <|> (endOfLine *> (turnPage <|> return ()))
  where
    pageBreak :: Parser Char
    pageBreak = char '\f'

    pageHeader :: Parser ()
    pageHeader = skipWhile (/= '\n') <* endOfInputOrLine *> many1 digit *> endOfInputOrLine

    turnPage :: Parser ()
    turnPage = pageBreak *> (pageHeader <|> endOfInput)

line :: Parser Text
line = takeWhile (/= '\n') <* endOfInputOrLineOrPage

hole :: Char
hole = '□'

skipEndingHoles :: Parser ()
skipEndingHoles = (skipManyHoles <|> pure ()) <* many (sep *> skipManyHoles)
  where
    skipManyHoles :: Parser ()
    skipManyHoles = char hole *> skipWhile1 (== hole)

    sep :: Parser ()
    sep = skipWhile1 (==' ') <|> skipWhile1 isAsciiAlphaNum

questionChar :: Parser Char
questionChar = satisfy (\c -> c /= '\n' && c /= hole)

questionSep :: Parser ()
questionSep = skipEndingHoles *> endOfInputOrLineOrPage

skipTillQuestions :: Parser [(Text, Text)]
skipTillQuestions = skipTill line questions

questions :: Parser [(Text, Text)]
questions =  (endOfInput *> return [])
         <|> (labelFst <$> counter <*> incompleteQuestionAndQuestions)
  where
    labelFst l (q, rest) = (l, q) : rest

incompleteQuestionAndQuestions :: Parser (Text, [(Text, Text)])
incompleteQuestionAndQuestions =
      (detectQuestionEnd *> (prependEmpty <$> skipTillQuestions))
  <|> (prependEmpty <$> questions)
  <|> (questionSep *> columns *> incompleteQuestionAndQuestions)
  <|> (questionSep *> incompleteQuestionAndQuestions)
  <|> (notes *> incompleteQuestionAndQuestions)
  <|> (consQuestion <$> questionChar <*> incompleteQuestionAndQuestions)
  where
    prependEmpty rest = ("", rest)

    consQuestion c (q, rest) = (T.cons c q, rest)

    detectQuestionEnd :: Parser ()
    detectQuestionEnd = choice
      [detectOptions, detectSpecializedOptions, range, detectSection]

    detectSection :: Parser ()
    detectSection = skipWhile1 (inClass "零壹貳參肆伍陸柒捌玖拾佰仟")
                 <* char '、'

    detectOptions :: Parser ()
    detectOptions = skipWhile (==' ')
                 *> char hole
                 *> notChar hole
                 *> skipWhile (/= '\n')
                 *> endOfInputOrLineOrPage

    detectSpecializedOptions :: Parser ()
    detectSpecializedOptions = skipWhile (==' ') *> choice (map helper keywords)
      where
        helper :: Text -> Parser ()
        helper keyword = string keyword *> detectOptions

        keywords :: [Text]
        keywords = ["最頂層", "受訪者自己算的："]

    range :: Parser ()
    range = skipWhile (==' ') *> many1 digit `sepBy2` char ' ' *> skipEndingHoles *> endOfInputOrLineOrPage

    -- "同不同意無所謂" 其實是 "無所謂同不同意"
    columnWords :: [String]
    columnWords =
      [ "非常同意同意不同意非常不同意不一定"
      , "非常同意同意同不同意無所謂不同意非常不同意無法選擇"
      , "非常同意同意同不同意無所謂不同意非常不同意"
      , "非常同意同意既不同意也不反對不同意非常不同意無法決定"
      , "非常同意同意既不同意也不反對不同意非常不同意"
      , "做全職做兼職在家中工作沒有工作不適用"
      , "做全職做兼職不要外出工作的想法尊重當事人自己無法選擇"
      , "總是母親做經常是母親做或一起做父母親做的一樣多經常是父親做總是父親做大都是女性做大都是小孩做其他家人或其他人做無法選擇"
      , "總是我經常是我或一起做兩人做的一樣多(或同居伴侶)經常是我的配偶(或同居伴侶)總是我的配偶由其他的人來做無法選擇不適用"
      , "總有幾次一星期總有幾次一個月有過一兩次從來沒有"
      , "經常有時很少從來沒有"
      , "很有感情有感情不太有感情完全沒有感情無法選擇"
      , "非常重要有點重要不怎麼重要一點也不重要無法決定"
      , "非常重要重要不重要非常不重要沒聽說過"
      , "非常光榮有些光榮不太光榮一點也不光榮無法決定"
      , "包括不包括看情形"
      , "有沒有"
      ]

    columns :: Parser ()
    columns = choice (map eatUp columnWords)
      where
        eatUp :: String -> Parser ()
        eatUp str = do
          sequence_ $ intersperse (endOfLine <|> skipWhile (==' ')) (map skipChar str)
          endOfInputOrLineOrPage

    notes :: Parser ()
    notes = void $  string "受訪者自己計算" *> skipWhile (==' ') *> string "訪員幫忙計算"
                <|> string "(訪員請唸出選項)"
                <|> string "（訪員請唸選項）"
                <|> string "（訪員請唸選項" *> skipWhile (==' ') *> many1 digit *> char '-' *> many1 digit *> string "）"
                <|> string "《訪員請唸出括號內的文字》"
                <|> string "不論父母存歿皆請續答 23、24"
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
    , parseOnly questions "1. X(2)Y" == Right [("1", "X"), ("2", "Y")]
    , parseOnly questions "1. X □(2)Y" == Right [("1", "X")]
    , parseOnly questions "1. X(註解)Y" == Right [("1", "X(註解)Y")]
    , parseOnly questions "1. X" == Right [("1", "X")]
    , parseOnly questions "X1. X□ □□" == Right [("X1", "X")]
    , parseOnly questions "F2. X □□ □□" == Right [("F2", "X")]
    , parseOnly questions "Z10a. X □Y □□\nZ10b. Z □□ □□\n" == Right [("Z10a", "X"), ("Z10b", "Z")]
    , parseOnly incompleteQuestionAndQuestions "\nA2. Y\n" == Right ("", [("A2", "Y")])
    , parseOnly questions "P1. X\nP2. Y\n" == Right [("P1", "X"), ("P2", "Y")]
    , parseOnly questions "a. A\nB\nC □□ □□\n\fXXX\n16\nb. D □\n" == Right [("a", "ABC"), ("b", "D")]
    , parseOnly questions "23.X" == Right [("23", "X")]
    , parseOnly questions "Z10a. X □Y □□\nZ10b. Z □□ □□\n" == Right [("Z10a", "X"), ("Z10b", "Z")]
    ]
