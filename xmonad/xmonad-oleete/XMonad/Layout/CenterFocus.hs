{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.TreeColumns
-- Copyright   :  (c) Kai Grossjohann <kai@emptydomain.de>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  ?
-- Stability   :  unstable
-- Portability :  unportable
--
-- A layout similar to tall but with fourcolumns. With 2560x1600 pixels this
-- layout can be used for a huge main window and up to six reasonable sized
-- stack windows.
-----------------------------------------------------------------------------

module XMonad.Layout.CenterFocus (
                              -- * Usage
                              -- $usage

                              -- * Screenshots
                              -- $screenshot
                              CenterFoc(..),
                              MirrorResize(..)
                             ) where

import XMonad
    ( fromMessage,
      splitHorizontally,
      splitVertically,
      Rectangle(Rectangle),
      LayoutClass(pureLayout, description, handleMessage),
      IncMasterN(IncMasterN),
      Resize(Expand, Shrink) )
import qualified XMonad.StackSet as W
import XMonad.Layout.ResizableTile(MirrorResize(..))

import Data.Ratio ( (%) )

import Control.Monad ( msum, ap )

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.CenterFocus
--
-- Then edit your @layoutHook@ by adding the CenterFoc layout:
--
-- > myLayout = CenterFoc 1 (3/100) (1/2) ||| CenterFocMid 1 (3/100) (1/2) ||| etc..
-- > main = xmonad def { layoutHook = myLayout }
--
-- The first argument specifies how many windows initially appear in the main
-- window. The second argument argument specifies the amount to resize while
-- resizing and the third argument specifies the initial size of the columns.
-- A positive size designates the fraction of the screen that the main window
-- should occupy, but if the size is negative the absolute value designates the
-- fraction a stack column should occupy. If both stack columns are visible,
-- they always occupy the same amount of space.
--
-- The CenterFocMid variant places the main window between the stack columns.
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"


-- $screenshot
-- <<http://server.c-otto.de/xmonad/CenterFocusMiddle.png>>

-- | Arguments are nmaster, delta, fraction
data CenterFoc a = CenterFocMid { centerFocNMaster :: !Int, centerFocDelta :: !Rational, centerFocFrac :: !Rational, centerFocMirrorFrac :: !Rational}
                | CenterFoc    { centerFocNMaster :: !Int, centerFocDelta :: !Rational, centerFocFrac :: !Rational, centerFocMirrorFrac :: !Rational}
    deriving (Show,Read)

instance LayoutClass CenterFoc a where
    pureLayout (CenterFoc n _ f mf) r    = doL False n f mf r
    pureLayout (CenterFocMid n _ f mf) r = doL True n f mf r
    handleMessage l m =
        return $ msum [fmap resize     (fromMessage m)
                      ,fmap mresize    (fromMessage m)
                      ,fmap incmastern (fromMessage m)]
            where resize Shrink = l { centerFocFrac = max (-0.5) $ f-d }
                  resize Expand = l { centerFocFrac = min 1 $ f+d }
                  mresize MirrorShrink = l { centerFocMirrorFrac = max (-0.5) $ mf-d }
                  mresize MirrorExpand = l { centerFocMirrorFrac = min 1 $ mf+d }
                  incmastern (IncMasterN x) = l { centerFocNMaster = max 0 (n+x) }
                  n = centerFocNMaster l
                  d = centerFocDelta l
                  f = centerFocFrac l
                  mf = centerFocMirrorFrac l
    description _ = "CenterFoc"

doL :: Bool-> Int-> Rational-> Rational -> Rectangle-> W.Stack a-> [(a, Rectangle)]
doL m n f mf r = ap zip (tile3 m f mf r n . length) . W.integrate

-- | tile3.  Compute window positions using 3 panes
tile3 :: Bool -> Rational -> Rational -> Rectangle -> Int -> Int -> [Rectangle]
tile3 middle f mf r nmaster n
    | n <= nmaster || nmaster == 0 = splitHorizontally n r1
    | otherwise = splitHorizontally nmaster r1 ++ splitVertically nstack1 r2 ++ splitHorizontally nstack2 r5 ++ rev (splitVertically nstack3 r3) ++ rev (splitHorizontally nstack4 r4)
        where (r0, r2, r3) = split3HorizontallyBy middle (if f<0 then 1+2*f else f) r
              (r1, r4, r5) = split3VerticallyBy (if mf<0 then 1+2*mf else mf) r0
              nstack = n - nmaster
              nstack1 = ceiling (nstack % 4)
              nstack2 = ceiling ((nstack - nstack1) % 3)
              nstack3 = ceiling ((nstack - nstack2 - nstack1) % 2)
              nstack4 = nstack - nstack1 - nstack2 - nstack3

rev :: [Rectangle] -> [Rectangle]
rev [] = []
rev x = last x : rev (init x)

split3HorizontallyBy :: Bool -> Rational -> Rectangle -> (Rectangle, Rectangle, Rectangle)
split3HorizontallyBy middle f (Rectangle sx sy sw sh) =
    if middle
    then ( Rectangle (sx + fromIntegral r3w) sy r1w sh
         , Rectangle sx sy r3w sh
         , Rectangle (sx + fromIntegral r3w + fromIntegral r1w) sy r2w sh )
    else ( Rectangle sx sy r1w sh
         , Rectangle (sx + fromIntegral r1w) sy r2w sh
         , Rectangle (sx + fromIntegral r1w + fromIntegral r2w) sy r3w sh )
        where r1w = ceiling $ fromIntegral sw * f
              r2w = ceiling ( (sw - r1w) % 2 )
              r3w = sw - r1w - r2w

split3VerticallyBy :: Rational -> Rectangle -> (Rectangle, Rectangle, Rectangle)
split3VerticallyBy f (Rectangle sx sy sw sh) =
     (Rectangle sx (sy + fromIntegral r3w) sw r1w
     , Rectangle sx sy sw r3w
     , Rectangle sx (sy + fromIntegral r3w + fromIntegral r1w) sw r2w )
        where r1w = ceiling $ fromIntegral sh * f
              r2w = ceiling ( (sh - r1w) % 2 )
              r3w = sh - r1w - r2w
