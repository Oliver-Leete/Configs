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
-- A layout similar to tall but with noteboooklayout. With 2560x1600 pixels this
-- layout can be used for a huge main window and up to six reasonable sized
-- slave windows.
-----------------------------------------------------------------------------

module XMonad.Layout.Noteslim (
                              -- * Usage
                              -- $usage

                              -- * Screenshots
                              -- $screenshot
                              Noteslim(..)
                             ) where

import XMonad
    ( fromMessage,
      splitHorizontally,
      splitVertically,
      splitHorizontallyBy,
      splitVerticallyBy,
      Rectangle(Rectangle, rect_height),
      LayoutClass(pureLayout, description, handleMessage),
      IncMasterN(IncMasterN),
      Resize(Expand, Shrink) )
import qualified XMonad.StackSet as W

import Data.Ratio ( (%) )

import Control.Monad ( msum, ap )
import Graphics.X11.Xlib ( Rectangle(..) )

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.NoteslimLayout
--
-- Then edit your @layoutHook@ by adding the Noteslimlayout:
--
-- > myLayout = Noteslim1 (3/100) (1/2) ||| NoteslimMid 1 (3/100) (1/2) ||| etc..
-- > main = xmonad def { layoutHook = myLayout }
--
-- The first argument specifies how many windows initially appear in the main
-- window. The second argument argument specifies the amount to resize while
-- resizing and the third argument specifies the initial size of the columns.
-- A positive size designates the fraction of the screen that the main window
-- should occupy, but if the size is negative the absolute value designates the
-- fraction a slave column should occupy. If both slave columns are visible,
-- they always occupy the same amount of space.
--
-- The NoteslimMid variant places the main window between the slave columns.
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"


-- $screenshot
-- <<http://server.c-otto.de/xmonad/NoteslimMiddle.png>>

-- | Arguments are nmaster, delta, fraction
data Noteslim a = NoteslimMid { noteslimMaster :: !Int, noteslimDelta :: !Rational, noteslimFrac :: !Rational}
                | Noteslim{ noteslimMaster :: !Int, noteslimDelta :: !Rational, noteslimFrac :: !Rational}
    deriving (Show,Read)

instance LayoutClass Noteslim a where
    pureLayout (Noteslim n _ f) r    = doL False n f r
    pureLayout (NoteslimMid n _ f) r = doL True n f r
    handleMessage l m =
        return $ msum [fmap resize     (fromMessage m)
                      ,fmap incmastern (fromMessage m)]
            where resize Shrink = l { noteslimFrac = max (-0.5) $ f-d }
                  resize Expand = l { noteslimFrac = min 1 $ f+d }
                  incmastern (IncMasterN x) = l { noteslimMaster = max 0 (n+x) }
                  n = noteslimMaster l
                  d = noteslimDelta l
                  f = noteslimFrac l
    description _ = "Noteslim"

doL :: Bool-> Int-> Rational-> Rectangle-> W.Stack a-> [(a, Rectangle)]
doL m n f r = ap zip (tile3 m f r n . length) . W.integrate

-- | tile3.  Compute window positions using 3 panes
tile3 :: Bool -> Rational -> Rectangle -> Int -> Int -> [Rectangle]
tile3 middle f r nmaster n
    | middle && n <= nmaster || nmaster == 0 = splitHorizontally n r
    | middle && n <= nmaster+1 = splitHorizontally nmaster s2 ++ splitHorizontally 1 s1
    | middle && n <= nmaster+2 = splitHorizontally nmaster r1 ++ splitVertically 1 r2 ++ splitVertically 1 r3
    | middle && n <= nmaster+3 = splitHorizontally nmaster r1 ++ splitVertically 1 r21 ++ splitVertically 1 r3 ++ splitVertically 1 r22
    | middle && n <= nmaster+5 = splitHorizontally nmaster r11 ++ splitVertically 1 r21 ++ splitVertically 1 r3 ++ splitHorizontally (n-nmaster-2) r212
    | middle && n >= nmaster+6 = splitHorizontally nmaster t1 ++ splitVertically 1 t2 ++ splitVertically 1 t3 ++ splitHorizontally (n-nmaster-2) q2
    | not middle && n <= nmaster || nmaster == 0 = splitHorizontally n r
    | not middle && n <= nmaster+1 = splitHorizontally nmaster s1 ++ splitHorizontally 1 s2
    | not middle && n <= nmaster+2 = splitHorizontally nmaster r1 ++ splitVertically 1 r2 ++ splitVertically 1 r3
    | not middle && n <= nmaster+4 = splitHorizontally nmaster r11 ++ splitVertically 1 r2 ++ splitVertically 1 r3 ++ splitHorizontally (n-nmaster-2) r12
    | not middle && n <= nmaster+5 = splitHorizontally nmaster r11 ++ splitVertically 1 r21 ++ splitVertically 1 r3 ++ splitHorizontally (n-nmaster-2) r122
    | not middle && n >= nmaster+6 = splitHorizontally nmaster t1 ++ splitVertically 1 t2 ++ splitVertically 1 t3 ++ splitHorizontally (n-nmaster-2) q2
        where (r1, r2, r3) = split3HorizontallyBy middle (if f<0 then 1+2*f else f) r
              (s1, s2) = splitHorizontallyBy (if f<0 then 1+f else f) r

              (r11, r12) = splitVerticallyBy (2/3) r1
              (r21, r22) = splitVerticallyBy (2/3) r2
              (r31, r32) = splitVerticallyBy (2/3) r3

              r212 = rectangleDiff r22 r12
              r122 = rectangleDiff r12 r22

              (t1, t2, t3) = split3HorizontallyBy middle (if f<0 then 1+2*f else f) q1
              (q1, q2) = splitVerticallyBy (2/3) r

              nslave = n - nmaster
              nslave1 = ceiling (nslave % 2)
              nslave2 = n - nmaster - nslave1

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

rectangleDiff :: Rectangle -> Rectangle -> Rectangle
rectangleDiff firstRect secondRect =
    Rectangle (rect_x firstRect) (rect_y firstRect) (rect_width firstRect + rect_width secondRect) (rect_height firstRect)

-- tabs = zip (w : reverse l ++ r) (repeat rec)
