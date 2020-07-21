{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE OverloadedStrings          #-}

module Hyper.Types
  ( App
  , Coords( Coords )
  , Error(..)
  , GameOverModel(..)
  , GlobalBoard
  , Grid
  , LocalBoard
  , Model(..)
  , Opponent(..)
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
  , runApp
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

data Tie = Tie
  deriving (Eq, Generic, Show)

data GameOverModel = GameOverModel
  { player_      :: PlayerName
  , opponent_    :: Opponent
  , winner       :: Either Tie XO
  , globalBoard_ :: GlobalBoard
  } deriving (Eq, Generic, Show)

newtype Error = Error { unError :: Text }
  deriving (Eq, Generic, Show)

type GlobalBoard = Grid LocalBoard

type LocalBoard = Grid Spot

type Grid a = Three (Three a)

type Three a = (a, a, a)

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
