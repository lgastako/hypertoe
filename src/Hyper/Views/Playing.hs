{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Hyper.Views.Playing ( view ) where

import           Hyper.Prelude                          hiding ( view )

import           Hyper.Types
import qualified Hyper.Views.GlobalBoard as GlobalBoard
import           Shpadoinkle

view :: Applicative (Html m) => PlayingModel -> Html m Model
view m = GlobalBoard.view (m ^. #globalBoard) *> pure (Playing m)
