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

module XMonad.Layout.FourColumns (
                              -- * Usage
                              -- $usage

                              -- * Screenshots
                              -- $screenshot
                              FourCol(..),
                              ToggleMid(..)
                             ) where

import XMonad
    ( fromMessage,
      splitHorizontally,
      splitVertically,
      splitVerticallyBy,
      Rectangle(Rectangle),
      LayoutClass(pureLayout, description, handleMessage),
      IncMasterN(IncMasterN),
      Resize(Expand, Shrink) , Typeable)
import qualified XMonad.StackSet as W
import XMonad.Core (Message)

import Data.Ratio

import Control.Monad ( msum, ap )

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.FourColumns
--
-- Then edit your @layoutHook@ by adding the FourCol layout:
--
-- > myLayout = FourCol 1 (3/100) (1/2) ||| FourColMid 1 (3/100) (1/2) ||| etc..
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
-- The FourColMid variant places the main window between the stack columns.
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"


-- $screenshot
-- <<http://server.c-otto.de/xmonad/FourColumnsMiddle.png>>

data ToggleMid = ToggleMid deriving (Read, Show, Typeable)
instance Message ToggleMid

-- | Arguments are nmaster, delta, fraction
data FourCol a = FourCol { fourColMid :: !Bool, fourColNMaster :: !Int, fourColDelta :: !Rational, fourColFrac :: !Rational}
    deriving (Show,Read)

instance LayoutClass FourCol a where
    pureLayout (FourCol mid n _ f) r    = doL mid n f r
    handleMessage l m =
        return $ msum [fmap resize     (fromMessage m)
                      ,fmap incmastern (fromMessage m)
                      ,fmap toggleside (fromMessage m)]
            where resize Shrink = l { fourColFrac = max (-0.5) $ f-d }
                  resize Expand = l { fourColFrac = min 1 $ f+d }
                  incmastern (IncMasterN x) = l { fourColNMaster = max 0 (n+x) }
                  toggleside ToggleMid = l { fourColMid = not mid }
                  mid = fourColMid l
                  n = fourColNMaster l
                  d = fourColDelta l
                  f = fourColFrac l
    description _ = "FourCol"

doL :: Bool-> Int-> Rational-> Rectangle-> W.Stack a-> [(a, Rectangle)]
doL m n f r = ap zip (tile3 m f r n . length) . W.integrate

-- | tile3.  Compute window positions using 3 panes
tile3 :: Bool -> Rational -> Rectangle -> Int -> Int -> [Rectangle]
tile3 middle f r nmaster n
    | n <= nmaster || nmaster == 0 = map (`modY` r) (splitHorizontally n r)
    | middle && n <= nmaster+1 = map (`modY` r) (splitHorizontally nmaster s1 ++ [s2])
    | not middle && n <= nmaster+1 = map (`modY` r) (splitHorizontally nmaster s1 ++ [s2])
    | n <= nmaster+4 = splitHorizontally nmaster r1 ++ [r2] ++ splitVertically (n-nmaster-1) r3
    | n <= nmaster+6 = splitHorizontally nmaster r1 ++ [r21, r22] ++ splitVertically (n-nmaster-2) r3
    | n <= nmaster+7 = splitHorizontally nmaster r1 ++ [r23] ++ splitVertically 2 r24 ++ splitVertically (n-nmaster-3) r3
    | otherwise = splitHorizontally nmaster r1 ++ splitVertically nstack2 r2 ++ splitVertically nstack1 r3
        where (r1, r2, r3) = split3HorizontallyBy middle (if f<0 then 1+2*f else f) r
              (s1, s2) = split2HorizontallyBy middle (if f<0 then 1+f else f) r
              (r21, r22) = splitVerticallyBy (2/3) r2
              (r23, r24) = splitVerticallyBy (1/2) r2
              nstack = n - nmaster
              nstack1 = ceiling (nstack % 2)
              nstack2 = n - nmaster - nstack1

split2HorizontallyBy :: Bool -> Rational -> Rectangle -> (Rectangle, Rectangle)
split2HorizontallyBy middle f (Rectangle sx sy sw sh) =
    if middle
    then ( modY (Rectangle (sx + fromIntegral r2w) sy r1w sh) (Rectangle sx sy sw sh)
         , modY (Rectangle sx sy r2w sh) (Rectangle sx sy sw sh))
    else ( modY (Rectangle sx sy r1w sh) (Rectangle sx sy sw sh)
         , modY (Rectangle (sx + fromIntegral r1w) sy r2w sh) (Rectangle sx sy sw sh))
        where r1w = ceiling $ fromIntegral sw * f
              r2w = sw - r1w

split3HorizontallyBy :: Bool -> Rational -> Rectangle -> (Rectangle, Rectangle, Rectangle)
split3HorizontallyBy middle f (Rectangle sx sy sw sh) =
    if middle
    then ( modY (Rectangle (sx + fromIntegral r3w) sy r1w sh) (Rectangle sx sy sw sh)
         , modY (Rectangle sx sy r3w sh) (Rectangle sx sy sw sh)
         , modY (Rectangle (sx + fromIntegral r3w + fromIntegral r1w) sy r2w sh ) (Rectangle sx sy sw sh))
    else ( modY (Rectangle sx sy r1w sh) (Rectangle sx sy sw sh)
         , modY (Rectangle (sx + fromIntegral r1w) sy r2w sh) (Rectangle sx sy sw sh)
         , modY (Rectangle (sx + fromIntegral r1w + fromIntegral r2w) sy r3w sh) (Rectangle sx sy sw sh))
        where r1w = ceiling $ fromIntegral sw * f
              r2w = ceiling ( (sw - r1w) % 2 )
              r3w = sw - r1w - r2w

modY :: Rectangle -> Rectangle -> Rectangle
modY (Rectangle sx sy sw sh) (Rectangle bx by bw bh)= 
    Rectangle sx y sw h
    where mod = if ((toInteger (fromIntegral sx + sw - 30)) < (toInteger $ bx + ceiling (1/4 * toRational bw))) || ((toInteger (20 + sx)) > (toInteger $ bx + ceiling (3/4 * toRational bw)))
              then 31
              else 0
          y = sy - mod
          h = sh + fromIntegral mod
