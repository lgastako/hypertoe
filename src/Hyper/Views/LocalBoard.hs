{-# LANGUAGE RankNTypes #-}
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
     -> Coords
     -> Html m PlayingModel
view m co = table borders [ tbody_ $ map renderRow [_1, _2, _3] ]
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
          & #lastMove ?~ co

    spotTaken = "Spot taken.  Please choose an open spot."

    grc = globalToLocalFromCoords co

globalToLocalFromCoords :: Coords -> ALens' GlobalBoard LocalBoard
globalToLocalFromCoords (Coords (r, c)) = selectO r . selectI c
  where
    -- surely there's a way to combine these?

    selectO :: Trey -> Lens' GlobalBoard (Three LocalBoard)
    selectO = \case
      One   -> _1
      Two   -> _2
      Three -> _3

    selectI :: Trey -> Lens' (Three LocalBoard) LocalBoard
    selectI = \case
      One   -> _1
      Two   -> _2
      Three -> _3

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
