{-# LANGUAGE NoImplicitPrelude #-}

module Hyper.Debug
  ( log
  ) where

import Hyper.Prelude hiding ( log
                            , trace
                            )

import Data.String          ( String )
import Debug.Trace          ( trace )

log :: Show a => String -> a -> a
log msg x = trace msg' x
  where
    msg' = msg ++ ": " ++ show x
