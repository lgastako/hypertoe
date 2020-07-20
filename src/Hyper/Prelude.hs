{-# LANGUAGE NoImplicitPrelude #-}

module Hyper.Prelude
  ( module X
  , cs
  , universe
  ) where

import Control.Lens          as X hiding ( (#)
                                         , (<.>)
                                         , Strict
                                         , from
                                         , simple
                                         , to
                                         , uncons
                                         , universe
                                         , unsnoc
                                         , view
                                         )
import Data.Aeson            as X        ( FromJSON
                                         , ToJSON
                                         , parseJSON
                                         , toJSON
                                         )
import Data.Data             as X        ( Data )
import Data.Data.Lens        as X
import Data.Generics.Labels  as X hiding ( Constructor )
import Data.Generics.Product as X hiding ( HasField
                                         , getField
                                         , list
                                         )
import Protolude             as X hiding ( (<.>)
                                         , Constructor
                                         , HasField
                                         , Strict
                                         , from
                                         , getField
                                         , list
                                         , option
                                         , to
                                         , uncons
                                         , unsnoc
                                         )

cs :: StringConv a b => a -> b
cs = strConv Lenient

universe :: (Enum a, Bounded a) => [a]
universe = [minBound..]
