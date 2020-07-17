{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Hyper.Views.SigningIn ( view ) where

import Hyper.Prelude    hiding ( view )

import Hyper.Types             ( Model( Playing
                                      , SigningIn
                                      )
                               , SigningInModel
                               , initPlaying
                               )
import Shpadoinkle             ( Html
                               , MonadJSM
                               , text
                               )
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
