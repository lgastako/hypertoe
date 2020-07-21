{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Hyper.Views.LocalBoard ( view ) where

import Hyper.Prelude    hiding ( div )

import Data.String             ( fromString )
import Hyper.Fns
import Hyper.Types      hiding ( winner )
import Shpadoinkle
import Shpadoinkle.Html hiding ( head )

view :: forall m. Applicative m
     => PlayingModel
     -> Coords
     -> Html m PlayingModel
view m globalCoords = div "board" $
  markMay ++ [ table tableStyle [ tbody_ $ map renderRow universe ] ]
  where
    markMay = case winner of
       Left Tie          -> [ winnersMark (Left Tie) ]
       Right Open        -> []
       Right (Closed xo) -> [ winnersMark (Right xo) ]

    renderRow :: Trey -> Html m PlayingModel
    renderRow r = tr_ $ map renderCol universe
      where
        renderCol c = td
          [ onClick . maybeMove localCoords . clearErrors $ m ]
          [ div "cell" [ text . render $ spotAt localCoords ] ]
          where
            localCoords = Coords (r, c)

    spotAt pos = m
      ^. #globalBoard
      . cloneLens boardRc
      . cloneLens (localToSpotFromCoords pos)

    tableStyle = [class' cls]
    cls = "local " <> fromMaybe "" backgroundClass

    maybeMove :: Coords -> PlayingModel -> PlayingModel
    maybeMove pos m'
      | winner /= Right Open                        = addError boardClosed m'
      | not (isOpen $ m ^. #globalBoard . spotLens) = addError spotTaken m'
      | not rightBoardTargeted                      = addError wrongBoard m'
      | otherwise = m'
          & #globalBoard . spotLens .~ Closed (m ^. #turn)
          & #turn %~ oppositePlayer
          & #lastMove ?~ (globalCoords, pos)
      where
        spotLens :: Lens' GlobalBoard Spot
        spotLens = cloneLens boardRc . cloneLens (localToSpotFromCoords pos)

        rightBoardTargeted
            | noTarget           = True
            | targetIsAlreadyWon = True
            | otherwise          = thisBoardIsProperTarget

    noTarget = null $ m ^. #lastMove

    targetIsAlreadyWon :: Bool
    targetIsAlreadyWon = case snd <$> m ^. #lastMove of
      Nothing -> False
      Just lc -> Right Open /= checkForWinner
        (m ^. #globalBoard . cloneLens (globalToLocalFromCoords lc))

    thisBoardIsProperTarget = Just globalCoords == (snd <$> m ^. #lastMove)

    spotTaken   = "Spot taken.  Please choose an open spot."
    wrongBoard  = "You must pick a spot on the target board."
    boardClosed = "This board is closed, please move on an open board."

    winner :: Either Tie Spot
    winner = checkForWinner $ m ^. #globalBoard ^# boardRc

    backgroundClass
      | winner == Right (Closed X) = Just "board-closed-x"
      | winner == Right (Closed O) = Just "board-closed-y"
      | winner == Left Tie         = Just "board-closed-tie"  -- TODO handle css
      | thisBoardIsProperTarget    = Just "board-open"
      | targetIsAlreadyWon         = Just "board-open"
      | otherwise                  = Nothing

    boardRc :: ALens' GlobalBoard LocalBoard
    boardRc = globalToLocalFromCoords globalCoords

winnersMark :: Either Tie XO -> Html m a
winnersMark result = div (fromString $ "winner winner-" ++ letter)
  [ text (cs letter) ]
  where
    letter = case result of
      Left Tie -> "C"
      Right xo -> show xo

-- surely there's a way to combine these?
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

-- surely there's a way to combine these?
localToSpotFromCoords :: Coords -> ALens' LocalBoard Spot
localToSpotFromCoords (Coords (r, c)) = selectO r . selectI c
  where
    -- surely there's a way to combine these?

    selectO :: Trey -> Lens' LocalBoard (Three Spot)
    selectO = \case
      One   -> _1
      Two   -> _2
      Three -> _3

    selectI :: Trey -> Lens' (Three Spot) Spot
    selectI = \case
      One   -> _1
      Two   -> _2
      Three -> _3

render :: Spot -> Text
render = \case
  Open     -> ""
  Closed v -> show v
