{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Hyper.Views.Root ( view ) where

import           Hyper.Prelude                      hiding ( div
                                                           , view
                                                           )

import           Hyper.Types                               ( Model( Playing
                                                                  , SigningIn
                                                                  )
                                                           )
import qualified Hyper.Views.Playing   as Playing
import qualified Hyper.Views.SigningIn as SigningIn
import           Shpadoinkle                               ( Html
                                                           , MonadJSM
                                                           , text
                                                           )
import           Shpadoinkle.Html

view :: MonadJSM m => Model -> Html m Model
view m' = div_
  [ div [ id' "actual" ]
    [ actual m'
    ]
  , br'_
  , br'_
  , br'_
  , br'_
  , div [ id' "debug" ]
    [ text (show m')
    ]
  ]
  where
    actual = \case
      SigningIn m -> SigningIn.view m
      Playing   m -> Playing.view m
