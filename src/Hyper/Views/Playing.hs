{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Hyper.Views.Playing ( view ) where

import           Hyper.Prelude                          hiding ( div
                                                               , view
                                                               )

import           Hyper.Types                                   ( Coords( Coords )
                                                               , Error
                                                               , Model( Playing )
                                                               , PlayingModel
                                                               , opponentName
                                                               , unError
                                                               )
import qualified Hyper.Views.GlobalBoard as GlobalBoard
import           Shpadoinkle                                   ( Html
                                                               , text
                                                               )
import           Shpadoinkle.Html

view :: Applicative m => PlayingModel -> Html m Model
view m = div_ $
  [ text $ "Player: " <> m ^. #player . #unPlayerName
  , text "             "
  , text $ "Opponent: " <> m ^. #opponent . opponentName . #unPlayerName
  ]
  ++ maybe [] renderLastMove (m ^. #lastMove)
  ++ map renderError (m ^. #errors) ++
  [ Playing <$> GlobalBoard.view m ]
  where
    renderError :: Error -> Html m a
    renderError e = div [ ("style", "color: red") ] [ text . unError $ e ]

    renderLastMove :: Coords -> [Html m a]
    renderLastMove (Coords (r, c)) = pure $
      div [ ("style", "color: blue") ] [ text . ("Last Move: " <>) $ show (r, c) ]
