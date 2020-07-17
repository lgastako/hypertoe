{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Hyper.Views.Playing ( view ) where

import           Hyper.Prelude                          hiding ( view )

import           Hyper.Types                                   ( Model( Playing )
                                                               , PlayingModel
                                                               , opponentName
                                                               )
import qualified Hyper.Views.GlobalBoard as GlobalBoard
import           Shpadoinkle                                   ( Html
                                                               , text
                                                               )
import           Shpadoinkle.Html

view :: Functor m => PlayingModel -> Html m Model
view m = div_
  [ text $ "Player: " <> m ^. #player . #unPlayerName
  , text "             "
  , text $ "Opponent: " <> m ^. #opponent . opponentName . #unPlayerName
  , Playing <$> GlobalBoard.view m
  ]
