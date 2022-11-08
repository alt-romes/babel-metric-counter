{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
module Parser where

import qualified System.IO.Utf8 as Utf8
import System.IO

import Data.Void
import Data.Time.Clock
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

type Topic = String
type Id = String

type Time = NominalDiffTime

data LogMsg
  = PublishRequest Time Topic String -- [[PUBLISH REQUEST]]
  | PublishReply Time Topic String
  | SubscriptionRequest Time Topic
  | SubscriptionReply Time Topic
  | Delivered Time Topic String -- [[DELIVERED]]:(TOPIC)(ID)
  | Ignore
  deriving (Show, Eq)


pTime :: Parser Time
pTime = secondsToNominalDiffTime . (/ 1000) . read <$> between (char '[') (char ']') (many digitChar)

logLine :: Parser LogMsg
logLine = (do
  t <- char 'I' *> pTime <* char ' ' <* takeUntil ':' <* char ' '
  ((choice [ PublishRequest t      <$ string "[[PUBLISH REQUEST]]:"      <*> parens identifier <*> parens identifier
           , PublishReply t        <$ string "[[PUBLISH REPLY]]:"        <*> parens identifier <*> parens identifier
           , SubscriptionRequest t <$ string "[[SUBSCRIPTION REQUEST]]:" <*> identifier
           , SubscriptionReply t   <$ string "[[SUBSCRIPTION REPLY]]:"   <*> identifier
           , Delivered t           <$ string "[[DELIVERED]]:"            <*> parens identifier <*> parens identifier
           ]) <* newline) <|> (Ignore <$ takeUntil '\n'))
  <|> (Ignore <$ takeUntil '\n')

    where
      parens = between (char '(') (char ')')
      identifier = many (alphaNumChar <|> char '_' <|> char '-')
      takeUntil x = takeWhileP Nothing (/= x) <* char x

parseFile :: FilePath -> IO [LogMsg]
parseFile f = do
  Utf8.withFile f ReadMode $ \h -> do
    txt <- hGetContents h
    case parse (many logLine) f txt of
      Left bundle -> fail (errorBundlePretty bundle)
      Right xs -> return xs
