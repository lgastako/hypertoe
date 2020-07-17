{-# LANGUAGE NoImplicitPrelude #-}

module Hyper.Prelude
  ( module X
  , universe
  ) where

import Control.Lens          as X hiding ( (<.>)
                                         , Strict
                                         , from
                                         , simple
                                         , to
                                         , uncons
                                         , universe
                                         , unsnoc
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


universe :: (Enum a, Bounded a) => [a]
universe = [minBound..]
