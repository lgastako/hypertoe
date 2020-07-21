{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Hyper.App ( app ) where

import Hyper.Prelude

import Hyper.Debug                 as Debug
import Hyper.Fns
import Hyper.Types
import Hyper.Views.Root            as Root
import Language.Javascript.JSaddle          ( JSM )
import Shpadoinkle                          ( MonadJSM )
import Shpadoinkle.Backend.ParDiff          ( runParDiff )
import Shpadoinkle.Html.Utils               ( getBody
                                            , setTitle
                                            )
import Shpadoinkle.Router

app :: JSM ()
app = do
  setTitle "HyperToe"
  fullPageSPA @SPA
    runApp
    runParDiff
    mkInitialState
    (Root.view . Debug.log "STATE")
    getBody
    listenForRouteChanges
    routes
  where
    mkInitialState :: MonadJSM m => Route -> m Model
    mkInitialState = withHydration (pure . start)

    listenForRouteChanges :: {- MonadJSM m => -} Route -> Model -> m Model
    listenForRouteChanges = pure . pur . const . start

    start :: Route -> Model
    start _ = initModel

    pur :: (a -> Model) -> m Model
    pur = panic "Main.pur"

    routes :: Route
    routes = panic "Main.routes"
