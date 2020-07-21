{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Hyper.Views.Root ( view ) where

import           Hyper.Types
import qualified Hyper.Views.CSS       as CSS
import qualified Hyper.Views.GameOver  as GameOver
import qualified Hyper.Views.Playing   as Playing
import qualified Hyper.Views.SigningIn as SigningIn
import           Shpadoinkle
import           Shpadoinkle.Html

view :: MonadJSM m => Model -> Html m Model
view m' = div_
  [ CSS.view
  , actual m'
  ]
  where
    actual = \case
      SigningIn m -> SigningIn.view m
      Playing   m -> Playing.view m
      GameOver  m -> GameOver.view m
