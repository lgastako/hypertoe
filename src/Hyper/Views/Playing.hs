{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Hyper.Views.Playing ( view ) where

import           Hyper.Prelude                          hiding ( div )

import           Hyper.Fns
import           Hyper.Types
import qualified Hyper.Views.GlobalBoard as GlobalBoard
import qualified Hyper.Views.PlayerBar   as PlayerBar
import           Shpadoinkle
import           Shpadoinkle.Html

view :: Applicative m => PlayingModel -> Html m Model
view m = div "wrapper" $
  PlayerBar.view (m ^. #player) (m ^. #opponent . opponentName)
  ++ [ div "whoseTurn"
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
