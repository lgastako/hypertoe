{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Hyper.Views.PlayerBar ( view ) where

import Hyper.Prelude    hiding ( div )

import Hyper.Types
import Shpadoinkle
import Shpadoinkle.Html

view :: PlayerName -> PlayerName -> [Html m a]
view plr opp =
  [ div "playerLabel"
    [ b_ [ "Player: " ]
    , span_ [ text $ plr ^. #unPlayerName ]
    ]
  , div "opponentLabel"
    [ b_ [ "Opponent: " ]
    , span_ [ text $ opp ^. #unPlayerName ]
    ]
  , br'_
  , br'_
  ]
