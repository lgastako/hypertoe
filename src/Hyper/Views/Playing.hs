{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Hyper.Views.Playing ( view ) where

import           Hyper.Prelude                          hiding ( div )

import           Hyper.Fns
import           Hyper.Types
import qualified Hyper.Views.GlobalBoard as GlobalBoard
import           Shpadoinkle
import           Shpadoinkle.Html

view :: Applicative m => PlayingModel -> Html m Model
view m = div "wrapper" $
  [ div "playerLabel"
    [ b_ [ "Player: " ]
    , span_ [ text $ m ^. #player . #unPlayerName ]
    ]
  , div "opponentLabel"
    [ b_ [ "Opponent: " ]
    , span_ [ text $ m ^. #opponent . opponentName . #unPlayerName ]
    ]
  , br'_
  , br'_
  , div "whoseTurn"
    [ b_ [ text $ show (m ^. #turn) ]
    , text "'s Turn"
    ]
  , br'_
  ]
  ++ [ checkForGlobalWinner <$> GlobalBoard.view m ]
  ++ if shouldRenderLastMove then map renderError (m ^. #errors) else []
  ++ maybe [] renderLastMove (m ^. #lastMove)
  where
    renderError :: Error -> Html m a
    renderError e = div [ ("style", "color: red") ] [ text . unError $ e ]

    renderLastMove :: (Coords, Coords) -> [Html m a]
    renderLastMove cc = pure $
      div [ ("style", "color: blue") ] [ text . ("Last Move: " <>) $ show cc ]

    shouldRenderLastMove = True
