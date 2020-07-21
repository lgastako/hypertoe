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

import Hyper.Prelude

import Hyper.Types
import Hyper.Debug as Debug

addError :: Text -> PlayingModel -> PlayingModel
addError e = #errors %~ (Error e:)

checkForGlobalWinner :: PlayingModel -> Model
checkForGlobalWinner m = case Debug.log "PROXY WINNER"
  . checkForWinner
  . Debug.log "PROXY BOARD"
  $ proxyBoard of
  Left Tie          -> transitionToWinner m (Left Tie)
  Right (Open)      -> Playing m
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

    diag = zipWith f [0..]
      where
        f n xs = fromMaybe (panic "FIND A BETTER WAY") . head . drop n $ xs

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

-- TODO Surely there's biplatey solution
makeProxyBoard :: GlobalBoard -> LocalBoard
makeProxyBoard gb =
  ( ( f $ gb ^. _1 . _1
    , f $ gb ^. _1 . _2
    , f $ gb ^. _1 . _3
    )
  , ( f $ gb ^. _2 . _1
    , f $ gb ^. _2 . _2
    , f $ gb ^. _2 . _3
    )
  , ( f $ gb ^. _3 . _1
    , f $ gb ^. _3 . _2
    , f $ gb ^. _3 . _3
    )
  )
  where
    f :: LocalBoard -> Spot
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
