{-# OPTIONS -Wall #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import OdbcUtils
import Test.Tasty
import Test.Tasty.HUnit
--import Control.Lens
import Control.Lens.Error
import Database.ODBC.SQLServer

main :: IO ()
main = defaultMain $ testGroup "OdbcUtils"
     [
        testCase "dir in file" $ (@?=) (Success (IntValue 13)) ([RData [] [[IntValue 13]]] ^&? row1col1)
     ]

