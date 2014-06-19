{-# LANGUAGE OverloadedStrings,RankNTypes #-}

module API.IB.Parse where

import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString.Char8               (ByteString)
import qualified Data.ByteString.Char8               as BC
import           Data.Char                           (toLower)
import           Data.Map                            (Map)
import qualified Data.Map                            as Map (lookup)
import           Data.Maybe
import           Data.String
import           Data.Time
import           Data.Time.Clock.POSIX
import           Data.Time.Zones
import           Prelude                             hiding (takeWhile)

import           API.IB.Constant
import           API.IB.Util

-- -----------------------------------------------------------------------------

parseField :: Parser a -> Parser a
parseField p = p <* char sepC

parseEmptyField :: Parser ()
parseEmptyField = return () <* parseField (string "")

parseMaybeEmptyField :: (String -> Maybe a) -> Parser (Maybe a)
parseMaybeEmptyField = parseField . parseMaybeEmpty

parseIntField :: Parser Int
parseIntField = parseField decimal

parseIntField' :: Parser Int
parseIntField' = do
  s <- parseStringField
  return $ if s == "" then 0 else read s 

parseSignedIntField :: Parser Int
parseSignedIntField = parseField $ signed decimal

parseMaybeIntField :: Parser (Maybe Int)
parseMaybeIntField = parseMaybeEmptyField $ Just . read 

parseByteStringField :: Parser ByteString
parseByteStringField = parseField parseByteString

parseStringField :: Parser String
parseStringField = parseField parseString

parseMaybeStringField :: Parser (Maybe String)
parseMaybeStringField = parseMaybeEmptyField Just

parseDoubleField :: Parser Double
parseDoubleField = parseField double

parseDoubleField' :: Parser Double
parseDoubleField' = do
  s <- parseStringField
  return $ if s == "" then 0.0 else read s 

parseMaybeDoubleField :: Parser (Maybe Double)
parseMaybeDoubleField = parseMaybeEmptyField $ Just . read 

parseMaybeDoubleField' :: Parser (Maybe Double)
parseMaybeDoubleField' = parseMaybeEmptyField $ \s -> let d = read s in if show d == "Infinity" then Nothing else Just d 

parseSignedDoubleField :: Parser Double
parseSignedDoubleField = parseField $ signed double

parseBoolBinaryField :: Parser Bool
parseBoolBinaryField = fmap ('1' ==) $ parseField (char '0') <|> parseField (char '1')

parseBoolStringField :: Parser Bool
parseBoolStringField = (\v -> "true" == map toLower (BC.unpack v)) <$> parseField (string "True" <|> string "true" <|> string "False" <|> string "false")

parseUTCTimeField :: Parser UTCTime
parseUTCTimeField = (posixSecondsToUTCTime . realToFrac) <$> parseIntField 

parseByteString :: Parser ByteString
parseByteString = takeWhile (/= sepC)

parseString :: Parser String
parseString = BC.unpack <$> takeWhile (/= sepC)

parseMaybe :: Parser a -> Parser (Maybe a)
parseMaybe p = 
  Just <$> p <|>
  return Nothing <* parseStringField

parseMaybeEmpty :: (String -> Maybe a) -> Parser (Maybe a)
parseMaybeEmpty f = do
  s <- parseString
  return $ if s == "" then Nothing else f s 

parseStringToEnum :: (Read a) => Parser a
parseStringToEnum = do
  s <- parseString
  case stringToEnum s of
    Just s' -> return s'
    Nothing -> fail ""

parseTZ :: Map String TZ -> Parser TZ
parseTZ tzs = do
  s <- parseString 
  let tz = Map.lookup s tzs 
  maybe (fail "") return tz 




