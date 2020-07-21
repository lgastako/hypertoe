{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Hyper.Views.GameOver ( view ) where

-- import Hyper.Prelude

import Hyper.Types
import Shpadoinkle
-- import Shpadoinkle.Html

view :: GameOverModel -> Html m a
view _m = text "GAME OVER! MORE SOON!"
