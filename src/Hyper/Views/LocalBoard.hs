{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Hyper.Views.LocalBoard ( view ) where

-- import Hyper.Prelude hiding ( view )

import Hyper.Types
import Shpadoinkle

view :: LocalBoard -> Html m ()
view _b = text "LocalBoard.view"


  --text "Hyper.Views.Board"
  -- [ tbody_
  --   [ tr_
  --     [ td_ []
  --     , td_ []
  --     , td[]
  --     ]
  --   ]
  -- ]
