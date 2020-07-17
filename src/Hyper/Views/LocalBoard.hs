{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Hyper.Views.LocalBoard ( view ) where

import Hyper.Prelude    hiding ( view )

import Hyper.Types
import Shpadoinkle
import Shpadoinkle.Html

view :: Applicative m
     => PlayingModel -> ALens' GlobalBoard LocalBoard -> Html m PlayingModel
view m rc = table borders [ tbody_ $ map renderRow [_1, _2, _3] ]
  where
    renderRow r = tr borders $ map renderCol [_1, _2, _3]
      where
        renderCol c = td (borders ++ clicks)
          [ text (render $ lb ^. cloneLens r . cloneLens c) ]
          where
            clicks =
              [ onClick
                (m & #globalBoard
                  . cloneLens rc
                  . cloneLens r
                  . cloneLens c
                  .~ Closed (m ^. #turn)
                  & #turn %~ oppositePlayer
                )
              ]

    lb :: LocalBoard
    lb = m ^. #globalBoard ^# rc

    borders = [ ("style", "border: 1px solid red") ]

render :: Spot -> Text
render = \case
  Open     -> "_"
  Closed v -> show v

oppositePlayer :: XO -> XO
oppositePlayer = \case
  X -> O
  O -> X
