{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module API.IB.Parse where

import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString.Char8               (ByteString)
import qualified Data.ByteString.Char8               as BC
import           Data.Map                            (Map)
import qualified Data.Map                            as Map (lookup)
import           Data.Maybe
import           Data.Monoid                         (mconcat)
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
parseEmptyField = char sepC *> return ()

parseMaybeEmptyField :: Parser a -> Parser (Maybe a) 
parseMaybeEmptyField p = mconcat
  [ parseEmptyField *> return Nothing
  , Just <$> parseField p
  , parseStringField *> return Nothing
  ]

parseIntField :: Parser Int
parseIntField = parseField decimal

parseIntField' :: Parser Int
parseIntField' = (parseEmptyField *> return 0) <|> parseIntField

parseSignedIntField :: Parser Int
parseSignedIntField = parseField $ signed decimal

parseMaybeIntField :: Parser (Maybe Int)
parseMaybeIntField = parseMaybeEmptyField decimal

parseByteStringField :: Parser ByteString
parseByteStringField = parseField parseByteString

parseStringField :: Parser String
parseStringField = parseField parseString

parseMaybeStringField :: Parser (Maybe String)
parseMaybeStringField = parseMaybeEmptyField parseString

parseDoubleField :: Parser Double
parseDoubleField = parseField double

parseDoubleField' :: Parser Double
parseDoubleField' = (parseEmptyField *> return 0) <|> parseDoubleField

parseMaybeDoubleField :: Parser (Maybe Double)
parseMaybeDoubleField = parseMaybeEmptyField double

parseMaybeDoubleField' :: Parser (Maybe Double)
parseMaybeDoubleField' = 
  (string "1.7976931348623157E308" *> return Nothing) <|>
  parseMaybeDoubleField

parseSignedDoubleField :: Parser Double
parseSignedDoubleField = parseField $ signed double

parseBoolBinaryField :: Parser Bool
parseBoolBinaryField = parseField $ (char '0' *> return False) <|> (char '1' *> return True)

parseBoolStringField :: Parser Bool
parseBoolStringField = parseField $ (stringCI "false" *> return False) <|> (stringCI "true" *> return True)

parseUTCTimeField :: Parser UTCTime
parseUTCTimeField = (posixSecondsToUTCTime . realToFrac) <$> parseIntField 

parseByteString :: Parser ByteString
parseByteString = takeWhile (/= sepC)

parseString :: Parser String
parseString = BC.unpack <$> parseByteString

parseMaybe :: Parser a -> Parser (Maybe a)
parseMaybe p = (Just <$> p) <|> (parseStringField *> return Nothing)

parseStringToEnum :: (Read a) => Parser a
parseStringToEnum = do
  s <- parseString
  case stringToEnum s of
    Just s' -> return s'
    Nothing -> fail ""

parseStringToEnumField :: (Read a) => Parser a
parseStringToEnumField = parseField parseStringToEnum

parseDayYYYYMMDD :: Parser Day
parseDayYYYYMMDD = decimal >>= maybe empty return . decode
  where
  decode i =
    let year  = i `quot` 10000
        month = fromIntegral $ (i - year * 10000) `quot` 100
        day   = fromIntegral $ i `mod` 100
    in fromGregorianValid year month day

parseDayYYYYMMDD' :: Parser (Maybe Day)
parseDayYYYYMMDD' = parseMaybeEmptyField parseDayYYYYMMDD

parseTimeOfDayHHMMSS :: Parser TimeOfDay
parseTimeOfDayHHMMSS = do
  t <- makeTimeOfDayValid <$>
    (decimal <* char ':') <*>
    (decimal <* char ':') <*>
    (fromInteger <$> decimal)
  maybe empty return t

parseTimeOfDayHHMM :: Parser TimeOfDay
parseTimeOfDayHHMM = do
  t <- makeTimeOfDayValid <$>
    (decimal <* char ':') <*>
    decimal <*>
    pure 0
  maybe empty return t

parseTZ :: Map String TZ -> Parser TZ
parseTZ tzs = do
  s <- parseString 
  let tz = Map.lookup s tzs 
  maybe (fail "") return tz 




