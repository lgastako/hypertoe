{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE OverloadedStrings          #-}

module Hyper.Types
  ( App
  , Coords( Coords )
  , Error
  , GameOverModel(..)
  , GlobalBoard
  , Grid
  , LocalBoard
  , Model(..)
  , PlayerName(..)
  , PlayingModel(..)
  , Route(..)
  , SigningInModel(..)
  , SPA
  , Spot(..)
  , Tie(..)
  , Three
  , Trey(..)
  , XO(..)
  , addError
  , initModel
  , initPlaying
  , opponentName
  , runApp
  , unError
  ) where

import Hyper.Prelude

import Control.Monad.Catch         ( MonadThrow )
import Control.Monad.IO.Unlift
import Language.Javascript.JSaddle ( JSM
                                   , askJSM
                                   , runJSM
                                   )
import Servant.API                 ( Raw )
import Shpadoinkle                 ( MonadJSM )

-- #ifndef ghcjs_HOST_OS
--                                      , MonadJSM
-- #endif

type SPA = Raw

data Model
  = SigningIn SigningInModel
  | Playing PlayingModel
  | GameOver GameOverModel
  deriving (Eq, Generic, Show)

initModel :: Model
-- initModel = SigningIn . SigningInModel $ "lgastako"
initModel = Playing . initPlaying $ "lgastako"

newtype SigningInModel = SigningInModel { name :: Text }
  deriving (Eq, Generic, Show)

data Trey
  = One
  | Two
  | Three
  deriving (Bounded, Enum, Eq, Generic, Show)

newtype Coords = Coords (Trey, Trey)
  deriving (Eq, Generic, Show)

data PlayingModel = PlayingModel
  { player      :: PlayerName
  , opponent    :: Opponent
  , globalBoard :: GlobalBoard
  , turn        :: XO
  , errors      :: [Error]
  , lastMove    :: Maybe (Coords, Coords)
  } deriving (Eq, Generic, Show)

initPlaying :: Text -> PlayingModel
initPlaying n = PlayingModel
  { player      = PlayerName n
  , opponent    = ComputerOpponent (PlayerName "Hal")
  , globalBoard = initGlobalBoard
    -- TODO remove me
                  & tempSetup
  , turn        = X
  , errors      = []
  , lastMove    = Nothing
  }

tempSetup :: GlobalBoard -> GlobalBoard
tempSetup =
  ( (_2 . _2) %~ ((_1 . _2) .~ Closed X)
    . ((_2 . _2) .~ Closed X)
    . ((_3 . _2) .~ Closed X)
  )
  .
  ( (_2 . _3) %~ ((_1 . _2) .~ Closed X)
    . ((_2 . _2) .~ Closed X)
    . ((_3 . _2) .~ Closed X)
  )
  .
  ( (_2 . _1) %~ ((_1 . _2) .~ Closed X)
    . ((_2 . _2) .~ Closed X)
    . ((_3 . _2) .~ Closed X)
  )
  .
  ( (_1 . _1) %~ ((_1 . _2) .~ Closed O)
    . ((_2 . _2) .~ Closed O)
    . ((_3 . _2) .~ Closed O)
  )
  .
  ( (_1 . _2) %~ ((_1 . _2) .~ Closed O)
    . ((_2 . _2) .~ Closed O)
    . ((_3 . _2) .~ Closed O)
  )
  .
  ( (_1 . _3) %~ ((_1 . _2) .~ Closed O)
    . ((_2 . _2) .~ Closed O)
    . ((_3 . _2) .~ Closed O)
  )

data Tie = Tie
  deriving (Eq, Generic, Show)

data GameOverModel = GameOverModel
  { player_   :: PlayerName
  , opponent_ :: Opponent
  , winner    :: Either Tie XO
  } deriving (Eq, Generic, Show)

addError :: Text -> PlayingModel -> PlayingModel
addError e = #errors %~ (Error e:)

newtype Error = Error { unError :: Text }
  deriving (Eq, Generic, Show)

type GlobalBoard = Grid LocalBoard

type LocalBoard = Grid Spot

type Grid a = Three (Three a)

type Three a = (a, a, a)

initGlobalBoard :: GlobalBoard
initGlobalBoard = pureGrid (pureGrid Open)

pureGrid :: a -> Grid a
pureGrid = pureThree . pureThree

pureThree :: a -> Three a
pureThree x = (x, x, x)

data Spot
  = Open
  | Closed XO
  deriving (Data, Eq, Generic, Show)

data XO = X | O
  deriving (Data, Eq, Generic, Show)

newtype PlayerName = PlayerName { unPlayerName :: Text }
  deriving (Eq, FromJSON, Generic, Show, ToJSON)

data Opponent
  = KnownRemoteOpponent PlayerName
  | RandomRemoteOpponent PlayerName
  | ComputerOpponent PlayerName
  deriving (Eq, Generic, Show)

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

data Route = Home
  deriving (Eq, Generic, Show)

newtype App a = App { runApp :: JSM a }
  deriving ( Applicative
           , Functor
           , Monad
           , MonadIO
           , MonadThrow
-- #ifndef ghcjs_HOST_OS
           , MonadJSM
-- #endif
           )

instance MonadUnliftIO App where
  {-# INLINE askUnliftIO #-}
  askUnliftIO = do ctx <- askJSM; pure $ UnliftIO $ \(App m) -> runJSM m ctx

instance FromJSON Coords
instance FromJSON Error
instance FromJSON GameOverModel
instance FromJSON Model
instance FromJSON Opponent
instance FromJSON PlayingModel
instance FromJSON SigningInModel
instance FromJSON Spot
instance FromJSON Tie
instance FromJSON Trey
instance FromJSON XO

instance ToJSON Coords
instance ToJSON Error
instance ToJSON GameOverModel
instance ToJSON Model
instance ToJSON Opponent
instance ToJSON PlayingModel
instance ToJSON SigningInModel
instance ToJSON Spot
instance ToJSON Tie
instance ToJSON Trey
instance ToJSON XO
