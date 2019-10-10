{-# OPTIONS -Wall #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import OdbcUtils
import EasyTest
import Control.Lens
import Control.Lens.Error
import Database.ODBC.SQLServer
import Data.Maybe

main :: IO ()
main =
  run $ tests [
        scope "dir in file" $ expectEq (Success (IntValue 13)) ([RData [] [[IntValue 13]]] ^&? row1col1)
     ]

