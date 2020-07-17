{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import Hyper.Prelude

import Hyper.App     ( app )
import Shpadoinkle   ( runJSorWarp )

main :: IO ()
main = do
  putText $ "HyperToe Listening at http://localhost:" <> show port
  runJSorWarp port app
  where
    port = 8088
