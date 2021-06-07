{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.Simplest
-- Copyright   :  (c) 2007 Andrea Rossato
-- License     :  BSD-style (see xmonad/LICENSE)
--
-- Maintainer  :  andrea.rossato@unibz.it
-- Stability   :  unstable
-- Portability :  unportable
--
-- A very simple layout. The simplest, afaik.
-----------------------------------------------------------------------------

module XMonad.Layout.SimpleFocus
    ( -- * Usage:
      -- $usage
      SimpleFocus(..)
    ) where

import XMonad
import qualified XMonad.StackSet as S
import Data.Ratio
import XMonad.Core (Message)
import Control.Monad ( msum, ap )
-- $usage
-- You can use this module with the following in your
-- @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.Simplest
--
-- Then edit your @layoutHook@ by adding the Simplest layout:
--
-- > myLayout = Simplest ||| Full ||| etc..
-- > main = xmonad def { layoutHook = myLayout }
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"

data SimpleFocus a = SimpleFocus {focusFrac :: !Rational, focusDelta :: !Rational} deriving (Show, Read)
instance LayoutClass SimpleFocus a where
    pureLayout (SimpleFocus f d) rec (S.Stack w l r) = zip (w : reverse l ++ r) (repeat r1)
      where (r1, r2, r3) = split3HorizontallyBy (if f<0 then 1+2*f else f) rec
    handleMessage l m = 
      return $ msum [fmap resize (fromMessage m)]
      where resize Shrink = l { focusFrac = max (-0.5) $ f-d}
            resize Expand = l { focusFrac = min 1 $ f+d}
            f = focusFrac l
            d = focusDelta l

split3HorizontallyBy :: Rational -> Rectangle -> (Rectangle, Rectangle, Rectangle)
split3HorizontallyBy f (Rectangle sx sy sw sh) =
    ( Rectangle (sx + fromIntegral r3w) sy r1w sh
    , Rectangle sx sy r3w sh
    , Rectangle (sx + fromIntegral r3w + fromIntegral r1w) sy r2w sh )
        where r1w = ceiling $ fromIntegral sw * f
              r2w = ceiling ( (sw - r1w) % 2 )
              r3w = sw - r1w - r2w
