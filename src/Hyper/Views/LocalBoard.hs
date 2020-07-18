{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hyper.Views.LocalBoard ( view ) where

import Hyper.Prelude    hiding ( view )

import Data.String             ( fromString )
import Hyper.Types
import Shpadoinkle
import Shpadoinkle.Html hiding ( head )

-- TODO: replace [_1, _2, _3] with `universe` over `Trey` then another thing
-- like globalToLocalFromCoords to get to the lenses...then set the last move
-- to the local move, not the global move

view :: forall m. Applicative m
     => PlayingModel
     -> Coords
     -> Html m PlayingModel
view m globalCoords = table tableStyle [ tbody_ $ map renderRow universe ]
  where
    renderRow :: Trey -> Html m PlayingModel
    renderRow r = tr borderStyle $ map renderCol universe
      where
        renderCol c = td
          (borderStyle ++ [ onClick . maybeMove localCoords . clearErrors $ m ])
          [ text . render $ spotAt localCoords ]
          where
            localCoords = Coords (r, c)

    spotAt pos = m
      ^. #globalBoard
      . cloneLens boardRc
      . cloneLens (localToSpotFromCoords pos)

    tableStyle  = fromRaw $ addBackground <<$>> rawBorders
    borderStyle = fromRaw rawBorders
    rawBorders  = [ ("style", "border: 1px solid red;") ]

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

        rightBoardTargeted = null (m ^. #lastMove)
          || Just globalCoords == fmap snd (m ^. #lastMove)

    spotTaken   = "Spot taken.  Please choose an open spot."
    wrongBoard  = "You must pick a spot on the target board."
    boardClosed = "This board is closed, please move on an open board."

    winner :: Spot
    winner = checkForWinner $ m ^. #globalBoard ^# boardRc

    addBackground
      | winner == Closed X = (<> "background-color: #FF6347;")
      | winner == Closed O = (<> "background-color: #0000FF;")
      | Just globalCoords == fmap snd (m ^. #lastMove) =
          (<> " background-color: #ADD8E6;")
      | otherwise = identity

    boardRc :: ALens' GlobalBoard LocalBoard
    boardRc = globalToLocalFromCoords globalCoords

    fromRaw = (fmap . fmap) fromString

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
  Open     -> "_"
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
