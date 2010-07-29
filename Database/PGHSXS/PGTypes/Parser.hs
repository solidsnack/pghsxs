{-# LANGUAGE OverloadedStrings
  #-}


module Database.PGHSXS.PGTypes.Parser where

import Prelude hiding (takeWhile)
import Control.Applicative ((<$>), (<*>), pure, some)
import Data.ByteString (append, ByteString)

import Text.CSV.ByteString
import Data.Attoparsec.Char8


parse_table_info             =  manyTill parse_table_info_line empty_ending
 where
  empty_ending               =  many endOfLine >> endOfInput

parse_table_info_line        =  do
  some (char ' ')
  name                      <-  typ_column_name
  separator
  typ_string                <-  manyTill anyChar separator
  not_null                  <-  option False (string "not null" >> pure True)
  endOfLine
  pure (name, typ_string, not_null)
 where
  typ_column_name            =  do
    string "typ"
    append "typ" <$> takeWhile (isDigit .|| isAlpha_ascii)
  separator                  =  some (char ' ') >> char '|' >> some (char ' ')
    


(.||)                       ::  (a -> Bool) -> (a -> Bool) -> (a -> Bool)
f .|| g                      =  h  where  h a = f a || g a



