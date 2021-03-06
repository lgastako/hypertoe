{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Hyper.Views.CSS ( view ) where

import Hyper.Prelude    hiding ( position
                               , rem
                               , zoom
                               )

import Clay
import Shpadoinkle
import Shpadoinkle.Html        ( style_ )

view :: Html m a
view = style_ [ text (cs . renderWith pretty [] $ styleSheet) ]

styleSheet :: Css
styleSheet = do
  html ? do
    "zoom" -: "150%"

  ".board-closed-x" ? backgroundColor "#FF6347"
  ".board-closed-y" ? backgroundColor "#0000FF"
  ".board-open"     ? backgroundColor "#ADD8E6"

  table # ".global" ? do
    let sz = 3
    tr ? border solid (px sz) black
    td ? border solid (px sz) black

  table # ".local" ? do
    let sz = 1
    tr    ? border solid (px sz) red
    td    ? border solid (px sz) red

  ".cell" ? do
    let sz = 1
    height    $ rem sz
    width     $ rem sz
    textAlign center

  ".wrapper" ? do
    width (rem 16)
    "margin" -: "auto"

  ".whoseTurn" ? do
    width     $ pct 100
    textAlign center
    "margin" -: "auto"

  ".playerLabel" ? do
    "float" -: "left"

  ".opponentLabel" ? do
    "float" -: "right"

  ".board" ? position relative

  ".winner" ? do
    position  absolute
    textAlign center
    margin    _t _r _b _l
    color "#ffffff"
    "zoom" -: "500%"
      where
        _t = rem (-0.15)
        _l = rem 0.06
        _r = rem 0
        _b = rem 0
