{-# LANGUAGE OverloadedStrings
  #-}


module Database.PGHSXS.PGTypes.Parser where

import Prelude hiding (takeWhile)
import Control.Applicative ((<$>), (<*>), (<$), (<|>), pure, some)
import Data.ByteString (append, ByteString)

import Text.CSV.ByteString
import Data.Attoparsec.Char8


parse_table_info             =  manyTill parse_table_info_line empty_ending
 where
  empty_ending               =  endOfLine

parse_table_info_line        =  do
  some (char ' ')
  name                      <-  typ_column_name
  separator
  typ_string                <-  manyTill anyChar separator
  not_null                  <-  option False (string "not null" >> pure True)
  pure (name, typ_string, not_null)

typ_column_name =
  append <$> string "typ" <*> takeWhile (isDigit .|| isAlpha_ascii)

working = do
  char ' '
  name <- typ_column_name
  separator
  typ_string                <-  manyTill anyChar separator
  not_null <- takeWhile (/= '\n')
--not_null                  <-  ("not null\n" ?>> True) <|> ("\n" ?>> False)
--  The top one leads to a terminating parser. The bottom one does not.
  endOfLine
  return (name, typ_string, not_null)
 where
  bytes ?>> val = val <$ string bytes



separator                  =  some (char ' ') >> char '|' >> some (char ' ')
    


(.||)                       ::  (a -> Bool) -> (a -> Bool) -> (a -> Bool)
f .|| g                      =  h  where  h a = f a || g a



