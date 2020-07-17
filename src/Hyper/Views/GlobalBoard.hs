{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Hyper.Views.GlobalBoard ( view ) where

import           Hyper.Prelude                        hiding ( view )

import           Hyper.Types                                 ( Coords( Coords )
                                                             , PlayingModel
                                                             )
import qualified Hyper.Views.LocalBoard as LocalBoard
import           Shpadoinkle                                 ( Html )
import           Shpadoinkle.Html

view :: Applicative m => PlayingModel -> Html m PlayingModel
view m = table borders [ tbody_ $ map renderRow universe ]
  where
    renderRow r = tr borders $ map renderCol universe
      where
        renderCol c = td borders [ LocalBoard.view m (Coords (r, c)) ]

    borders = [ ("style", "border: 1px solid black") ]
