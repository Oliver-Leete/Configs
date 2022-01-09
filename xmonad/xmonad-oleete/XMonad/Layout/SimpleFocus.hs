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
import Control.Monad ( msum)
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

data SimpleFocus a = SimpleFocus {focusFrac :: !Rational, focusDelta :: !Rational, sizeLimit:: !Integer} deriving (Show, Read)
instance LayoutClass SimpleFocus a where
    pureLayout (SimpleFocus f _ limit) rec (S.Stack w l r) = zip (w : reverse l ++ r) (repeat midRect)
      where midRect = modY (centreRect (if f<0 then 1+2*f else f) limit rec)
    handleMessage l m =
      return $ msum [fmap resize (fromMessage m)]
      where resize Shrink = l { focusFrac = max (-0.5) $ f-d}
            resize Expand = l { focusFrac = min 1 $ f+d}
            f = focusFrac l
            d = focusDelta l

centreRect :: Rational -> Integer -> Rectangle -> Rectangle
centreRect f limit (Rectangle sx sy sw sh) =
        if width > fromIntegral limit
        then Rectangle (sx + gap) sy width sh
        else Rectangle (sx + limitGap) sy (fromIntegral limit) sh
        where width = ceiling $ fromIntegral sw * f
              gap = ceiling ( (sw - width) % 2 )
              limitGap = ceiling ( (sw - fromIntegral limit) % 2 )

modY :: Rectangle -> Rectangle
modY (Rectangle sx sy sw sh) =
    Rectangle sx y sw h
    where   ymoddifier= if toInteger (8 + sx) < 1280
                        then 31
                        else 0
            y = sy + ymoddifier
            h = sh - fromIntegral ymoddifier
