{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hyper.Views.LocalBoard ( view ) where

import Hyper.Prelude    hiding ( div )

import Hyper.Types
import Shpadoinkle
import Shpadoinkle.Html hiding ( head )

view :: forall m. Applicative m
     => PlayingModel
     -> Coords
     -> Html m PlayingModel
view m globalCoords = table tableStyle [ tbody_ $ map renderRow universe ]
  where
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
      | checkForWinner (m ^. #globalBoard . cloneLens boardRc) /= Open =
          addError boardClosed m'
      | not (isOpen $ m ^. #globalBoard . spotLens) =
          addError spotTaken m'
      | not rightBoardTargeted =
          addError wrongBoard m'
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
      Just lc -> Open /= (checkForWinner $
        (m ^. #globalBoard . cloneLens (globalToLocalFromCoords lc)))

    thisBoardIsProperTarget = Just globalCoords == (snd <$> m ^. #lastMove)

    spotTaken   = "Spot taken.  Please choose an open spot."
    wrongBoard  = "You must pick a spot on the target board."
    boardClosed = "This board is closed, please move on an open board."

    winner :: Spot
    winner = checkForWinner $ m ^. #globalBoard ^# boardRc

    backgroundClass
      | winner == Closed X      = Just "board-closed-x"
      | winner == Closed O      = Just "board-closed-y"
      | thisBoardIsProperTarget = Just "board-open"
      | targetIsAlreadyWon      = Just "board-open"
      | otherwise               = Nothing

    boardRc :: ALens' GlobalBoard LocalBoard
    boardRc = globalToLocalFromCoords globalCoords

checkForWinner :: LocalBoard -> Spot
checkForWinner lb = maybe Open Closed $ chk X <|> chk O
  where
    chk :: XO -> Maybe XO
    chk xo
      | any (all (== Closed xo)) rows  = Just xo
      | any (all (== Closed xo)) cols  = Just xo
      | any (all (== Closed xo)) diags = Just xo
      | otherwise                      = Nothing

    rows  = toListOf each <$> toListOf each lb
    cols  = transpose . map reverse $ rows
    diags = [diag rows, diag cols]

    diag = zipWith f [0..]
      where
        f n xs = fromMaybe (panic "FIND A BETTER WAY") . head . drop n $ xs

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

clearErrors :: PlayingModel -> PlayingModel
clearErrors = #errors .~ []

isOpen :: Spot -> Bool
isOpen = \case
  Open -> True
  _    -> False

render :: Spot -> Text
render = \case
  Open     -> ""
  Closed v -> show v

oppositePlayer :: XO -> XO
oppositePlayer = \case
  X -> O
  O -> X


_deleteMe :: LocalBoard
_deleteMe =
  ( (Open,     Open,     Open)
  , (Closed X, Closed O, Closed X)
  , (Open,     Closed X, Closed O)
  )

_deleteMe2 :: LocalBoard
_deleteMe2 =
  ( (Closed X, Open,     Open)
  , (Closed X, Closed O, Closed X)
  , (Closed X, Closed X, Closed O)
  )

_deleteMe3 :: LocalBoard
_deleteMe3 =
  ( (Closed X, Open,     Open)
  , (Closed O, Closed O, Closed O)
  , (Closed X, Closed X, Closed O)
  )

_deleteMe4 :: LocalBoard
_deleteMe4 =
  ( (Closed X, Open,     Open)
  , (Closed O, Closed X, Closed O)
  , (Closed X, Open,     Closed X)
  )

_deleteMe5 :: LocalBoard
_deleteMe5 =
  ( (Open,     Open,     Closed X)
  , (Closed O, Closed X, Closed O)
  , (Closed X, Open,     Open)
  )
