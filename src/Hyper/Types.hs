{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}

module Hyper.Types
  ( App
  , GlobalBoard
  , Grid
  , LocalBoard
  , Model(..)
  , PlayingModel(..)
  , Route(..)
  , SigningInModel(..)
  , SPA
  , Spot(..)
  , Three
  , initModel
  , initPlaying
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
  deriving (Eq, Generic, Show)

initModel :: Model
initModel = SigningIn . SigningInModel $ "lgastako"

data SigningInModel = SigningInModel
  { name :: Text
  } deriving (Eq, Generic, Show)

data PlayingModel = PlayingModel
  { player      :: PlayerName
  , opponent    :: Opponent
  , globalBoard :: GlobalBoard
  } deriving (Eq, Generic, Show)

initPlaying :: Text -> PlayingModel
initPlaying n = PlayingModel
  { player      = PlayerName n
  , opponent    = ComputerOpponent (PlayerName "Hal")
  , globalBoard = initGlobalBoard
  }

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

newtype PlayerName = PlayerName Text
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

instance FromJSON Model
instance FromJSON Opponent
instance FromJSON PlayingModel
instance FromJSON SigningInModel
instance FromJSON Spot
instance FromJSON XO

instance ToJSON Model
instance ToJSON Opponent
instance ToJSON PlayingModel
instance ToJSON SigningInModel
instance ToJSON Spot
instance ToJSON XO
