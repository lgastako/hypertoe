{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Hyper.Views.Playing ( view ) where

import           Hyper.Prelude                          hiding ( div )

import           Hyper.Types
import qualified Hyper.Views.GlobalBoard as GlobalBoard
import           Shpadoinkle
import           Shpadoinkle.Html

view :: Applicative m => PlayingModel -> Html m Model
view m = div_ $
  [ b_ [ "Player: " ]
  , span_ [ text $ m ^. #player . #unPlayerName ]
  , text "             "
  , b_ [ "Opponent: " ]
  , span_ [ text $ m ^. #opponent . opponentName . #unPlayerName ]
  , br'_
  , br'_
  , text $ show (m ^. #turn) <> "'s Turn to Move."
  , br'_
  , br'_
  ]
  ++ [ checkForWinner <$> GlobalBoard.view m ]
  ++ map renderError (m ^. #errors)
  ++ maybe [] renderLastMove (m ^. #lastMove)
  where
    renderError :: Error -> Html m a
    renderError e = div [ ("style", "color: red") ] [ text . unError $ e ]

    renderLastMove :: (Coords, Coords) -> [Html m a]
    renderLastMove cc = pure $
      div [ ("style", "color: blue") ] [ text . ("Last Move: " <>) $ show cc ]

    checkForWinner :: PlayingModel -> Model
    checkForWinner = Playing -- TODO: check for winner
