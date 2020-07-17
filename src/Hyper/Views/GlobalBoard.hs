{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Hyper.Views.GlobalBoard ( view ) where

import           Hyper.Prelude                        hiding ( view )

import           Hyper.Types
import qualified Hyper.Views.LocalBoard as LocalBoard
import           Shpadoinkle

view :: Applicative (Html m) => GlobalBoard -> Html m ()
view = traverse_ LocalBoard.view . toListOf biplate
