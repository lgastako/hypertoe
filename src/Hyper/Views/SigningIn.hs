{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Hyper.Views.SigningIn ( view ) where

import Hyper.Prelude

import Hyper.Fns
import Hyper.Types
import Shpadoinkle
import Shpadoinkle.Html

view :: MonadJSM m => SigningInModel -> Html m Model
view m = div_
  [ h1_ [ text "HyperToe" ]
  , form_
    [ text "Signing In"
    , br'_
    , input'
      [ type' "text"
      , onInput (\n -> SigningIn (m & #name .~ n))
      , value (m ^. #name)
      ]
    , br'_
    , button
      [ type' "submit"
      , onClick (Playing . initPlaying $ m ^. #name)
      ]
      [ text "Sign In"
      ]
    ]
  ]
