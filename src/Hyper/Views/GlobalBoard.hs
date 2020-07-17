{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Hyper.Views.GlobalBoard ( view ) where

import           Hyper.Prelude                        hiding ( view )

import           Hyper.Types                                 ( PlayingModel )
import qualified Hyper.Views.LocalBoard as LocalBoard
import           Shpadoinkle                                 ( Html )
import           Shpadoinkle.Html

view :: Applicative m => PlayingModel -> Html m PlayingModel
view m = table borders [ tbody_ $ map renderRow [_1, _2, _3] ]
  where
    renderRow rowLens = tr borders $ map renderCol [_1, _2, _3]
      where
        renderCol colLens = td borders [ LocalBoard.view m rc ]
          where
            rc = cloneLens rowLens . cloneLens colLens

    borders = [ ("style", "border: 1px solid black") ]
