{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Hyper.Fns
  ( addError
  , checkForGlobalWinner
  , checkForWinner
  , clearErrors
  , initModel
  , initPlaying
  , isOpen
  , makeProxyBoard
  , opponentName
  , oppositePlayer
  , playAgain
  ) where

import Prelude       ( (!!) )
import Hyper.Prelude

import Hyper.Types

addError :: Text -> PlayingModel -> PlayingModel
addError e = #errors %~ (Error e:)

checkForGlobalWinner :: PlayingModel -> Model
checkForGlobalWinner m = case checkForWinner proxyBoard of
  Left Tie          -> transitionToWinner m (Left Tie)
  Right Open        -> Playing m
  Right (Closed xo) -> transitionToWinner m (Right xo)
  where
    proxyBoard :: LocalBoard
    proxyBoard = makeProxyBoard $ m ^. #globalBoard

checkForWinner :: LocalBoard -> Either Tie Spot
checkForWinner lb = case maybe Open Closed $ chk X <|> chk O of
  Closed xo -> Right . Closed $ xo
  Open | not . any (== Open) $ allSpots -> Left Tie
       | otherwise -> Right Open
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

    diag = zipWith (flip (!!)) [0..]

    allSpots = lb ^.. biplate

clearErrors :: PlayingModel -> PlayingModel
clearErrors = #errors .~ []

initGlobalBoard :: GlobalBoard
initGlobalBoard = pureGrid (pureGrid Open)

initModel :: Model
-- initModel = SigningIn . SigningInModel $ "lgastako"
initModel = Playing . initPlaying $ "lgastako"

initPlaying :: Text -> PlayingModel
initPlaying n = PlayingModel
  { player      = PlayerName n
  , opponent    = ComputerOpponent (PlayerName "Hal")
  , globalBoard = initGlobalBoard
  , turn        = X
  , errors      = []
  , lastMove    = Nothing
  }

isOpen :: Spot -> Bool
isOpen = \case
  Open -> True
  _    -> False

-- TODO Can we write a polymorphic biplate to make this just something like:
--   gb & polyplate %~ f
makeProxyBoard :: GlobalBoard -> LocalBoard
makeProxyBoard gb = gb
  & _1 . _1 %~ f
  & _1 . _2 %~ f
  & _1 . _3 %~ f
  & _2 . _1 %~ f
  & _2 . _2 %~ f
  & _2 . _3 %~ f
  & _3 . _1 %~ f
  & _3 . _2 %~ f
  & _3 . _3 %~ f
  where
    f = either (const Open) identity . checkForWinner

opponentName :: Lens' Opponent PlayerName
opponentName = lens g s
  where
    g = \case
      KnownRemoteOpponent n  -> n
      RandomRemoteOpponent n -> n
      ComputerOpponent n     -> n

    s (KnownRemoteOpponent  _) n = KnownRemoteOpponent n
    s (RandomRemoteOpponent _) n = RandomRemoteOpponent n
    s (ComputerOpponent     _) n = ComputerOpponent n

oppositePlayer :: XO -> XO
oppositePlayer = \case
  X -> O
  O -> X

playAgain :: GameOverModel -> Model
playAgain gom = Playing . initPlaying $ gom ^. #player_ . #unPlayerName

pureGrid :: a -> Grid a
pureGrid = pureThree . pureThree

pureThree :: a -> Three a
pureThree x = (x, x, x)

transitionToWinner :: PlayingModel -> Either Tie XO -> Model
transitionToWinner m w = GameOver $ GameOverModel
  { player_      = m ^. #player
  , opponent_    = m ^. #opponent
  , winner       = w
  , globalBoard_ = m ^. #globalBoard
  }
