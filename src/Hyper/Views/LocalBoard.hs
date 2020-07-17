{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}

module Hyper.Views.LocalBoard ( view ) where

import Hyper.Prelude    hiding ( view )

import Hyper.Types
import Shpadoinkle
import Shpadoinkle.Html

view :: PlayingModel -> ALens' GlobalBoard LocalBoard -> Html m PlayingModel
view m rc = table borders [ tbody_ $ map renderRow [_1, _2, _3] ]
  where
    renderRow r = tr borders $ map renderCol [_1, _2, _3]
      where
        renderCol c = td borders [ text (show $ lb ^. cloneLens r . cloneLens c) ]

    lb :: LocalBoard
    lb = m ^. #globalBoard ^# rc

    borders = [ ("style", "border: 1px solid red") ]
