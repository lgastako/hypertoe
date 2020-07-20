{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Hyper.Views.GlobalBoard ( view ) where

import           Hyper.Prelude

import           Hyper.Types
import qualified Hyper.Views.LocalBoard as LocalBoard
import           Shpadoinkle
import           Shpadoinkle.Html

view :: Applicative m => PlayingModel -> Html m PlayingModel
view m = table "global" [ tbody_ $ map renderRow universe ]
  where
    renderRow r = tr_ $ map renderCol universe
      where
        renderCol c = td_ [ LocalBoard.view m (Coords (r, c)) ]
