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
     => PlayingModel
     -> ALens' GlobalBoard LocalBoard
     -> Html m PlayingModel
view m grc = table borders [ tbody_ $ map renderRow [_1, _2, _3] ]
  where
    renderRow r = tr borders $ map renderCol [_1, _2, _3]
      where
        renderCol c = td
          (borders ++ [ onClick . maybeMove (r, c) . clearErrors $ m ])
          [ text . render $ spotAt (r, c) ]

    lensAt (r, c) = cloneLens grc . cloneLens r . cloneLens c

    spotAt pos = m ^. #globalBoard . lensAt pos

    borders = [ ("style", "border: 1px solid red") ]

    maybeMove pos m'
      | not (isOpen $ m ^. #globalBoard . lensAt pos) = addError spotTaken m'
      | otherwise = m'
          & #globalBoard . lensAt pos .~ Closed (m ^. #turn)
          & #turn %~ oppositePlayer

    spotTaken = "Spot taken.  Please choose an open spot."

clearErrors :: PlayingModel -> PlayingModel
clearErrors = #errors .~ []

isOpen :: Spot -> Bool
isOpen = \case
  Open -> True
  _    -> False

render :: Spot -> Text
render = \case
  Open     -> "_"
  Closed v -> show v

oppositePlayer :: XO -> XO
oppositePlayer = \case
  X -> O
  O -> X
