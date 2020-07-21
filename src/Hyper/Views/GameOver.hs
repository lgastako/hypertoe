{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Hyper.Views.GameOver ( view ) where

import           Hyper.Prelude                      hiding ( div )

import           Hyper.Fns
import           Hyper.Types
import qualified Hyper.Views.PlayerBar as PlayerBar
import           Shpadoinkle
import           Shpadoinkle.Html

view :: Applicative m => GameOverModel -> Html m Model
view m = div "wrapper" $
  [ h1_ [ text "GAME OVER" ] ]
  ++ PlayerBar.view (m ^. #player_) (m ^. #opponent_ . opponentName)
  ++ [ h2_ [ text $ "Winner: " <> render (m ^. #winner) ]
     , button [ onClick (playAgain m) ]
       [ text "Play Again" ]
     ]

render :: Either Tie XO -> Text
render = either show show
