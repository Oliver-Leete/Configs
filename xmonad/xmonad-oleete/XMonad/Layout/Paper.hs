{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

{- |
Module      :  XMonad.Layout.Accordion
Description :  Put non-focused windows in ribbons at the top and bottom of the screen.
Copyright   :  (c) glasser@mit.edu
License     :  BSD

Maintainer  :  glasser@mit.edu
Stability   :  stable
Portability :  unportable

LayoutClass that puts non-focused windows in ribbons at the top and bottom
of the screen.
-}
module XMonad.Layout.Paper (
    -- * Usage
    -- $usage
    Paper (Paper),
) where

import Data.Ratio
import XMonad
import qualified XMonad.StackSet as W

{- $usage
You can use this module with the following in your @xmonad.hs@:

> import XMonad.Layout.Accordion

Then edit your @layoutHook@ by adding the Accordion layout:

> myLayout = Accordion ||| Full ||| etc..
> main = xmonad def { layoutHook = myLayout }

For more detailed instructions on editing the layoutHook see
<https://xmonad.org/TUTORIAL.html#customizing-xmonad the tutorial> and
"XMonad.Doc.Extending#Editing_the_layout_hook".
-}

newtype Paper a = Paper {expand :: Bool}
    deriving (Read, Show)

instance LayoutClass Paper Window where
    pureLayout (Paper toExpand) sc ws = zip ups tops ++ [(W.focus ws, mainPane)] ++ zip dns bottoms
      where
        ups = W.up ws
        dns = W.down ws
        (left, center, right) = split3HorizontallyBy (1 / 2) sc
        mainPane
            | toExpand && ups /= [] && dns /= [] = center
            | toExpand && ups /= [] = rectangleDiff center right
            | toExpand && dns /= [] = rectangleDiff left center
            | toExpand = sc
            | otherwise = center
        tops = [left | ups /= []]
        bottoms = [right | dns /= []]
    description _ = "Paper"

split3HorizontallyBy :: Rational -> Rectangle -> (Rectangle, Rectangle, Rectangle)
split3HorizontallyBy f (Rectangle sx sy sw sh) =
    ( modY (Rectangle sx sy sw sh) (Rectangle sx sy r3w sh)
    , modY (Rectangle sx sy sw sh) (Rectangle (sx + fromIntegral r3w) sy r1w sh)
    , modY (Rectangle sx sy sw sh) (Rectangle (sx + fromIntegral r3w + fromIntegral r1w) sy r2w sh)
    )
  where
    r1w = ceiling $ fromIntegral sw * f
    r2w = ceiling ((sw - r1w) % 2)
    r3w = sw - r1w - r2w

modY :: Rectangle -> Rectangle -> Rectangle
modY (Rectangle bx _ bw _) (Rectangle sx sy sw sh) = Rectangle sx (sy + ymod) sw (sh - fromIntegral ymod)
  where
    ymod = if toInteger (8 + sx) < toInteger bx + xmobarWidth then 31 else 0
    xmobarWidth = if bw > 1920 then 960 else 1280

rectangleDiff :: Rectangle -> Rectangle -> Rectangle
rectangleDiff (Rectangle sx sy sw sh) (Rectangle _ _ bw _) = Rectangle sx sy (sw + bw) sh
