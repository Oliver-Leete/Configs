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
-- stack windows.
-----------------------------------------------------------------------------

module XMonad.Layout.Notebook (
                              -- * Usage
                              -- $usage

                              -- * Screenshots
                              -- $screenshot
                              Notebook(..),
                              MirrorResize(..),
                              IncColumnN(..),
                              ToggleMiddle(..),
                              ToggleSide(..),
                              ToggleStackDir(..)
                             ) where

import XMonad
    ( fromMessage,
      splitHorizontally,
      splitVertically,
      splitHorizontallyBy,
      splitVerticallyBy,
      LayoutClass(pureLayout, description, handleMessage),
      IncMasterN(IncMasterN),
      Resize(Expand, Shrink), Typeable)
import qualified XMonad.StackSet as W
import XMonad.Layout.ResizableTile(MirrorResize(..))

import Data.Ratio ( (%) )

import Control.Monad ( msum )
import Graphics.X11.Xlib ( Rectangle(..) )
import Data.List (sortOn)
-- import qualified Basement.Compat.Base as GHC.Int
import XMonad.Core (Message)
import qualified Data.Ord
-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.NotebookLayout
--
-- Then edit your @layoutHook@ by adding the Notebooklayout:
--
-- > myLayout = Notebook1 (3/100) (1/2) ||| NotebookMid 1 (3/100) (1/2) ||| etc..
-- > main = xmonad def { layoutHook = myLayout }
--
-- The first argument specifies how many windows initially appear in the main
-- window. The second argument argument specifies the amount to resize while
-- resizing, the third argument specifies the initial size of the columns and
-- the fourth argument specifies the initial size of the rows.
-- A positive size designates the fraction of the screen that the main window
-- should occupy, but if the size is negative the absolute value designates the
-- fraction a stack column should occupy. If both stack columns are visible,
-- they always occupy the same amount of space.
--
-- To help with the mess of variables below, the letter corisponds to the
-- sublayout (number of main windows, with a being two), the first number
-- corisponds to the column number and the third corisponds to the row number.
-- For three digit ones the first two numbers corispond to the first and second
-- rectangle that where combined and the last to the row (always two, but kept to
-- stop variable clashes)
--
-- The NotebookMid variant places the main window between the stack columns.
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"


-- $screenshot
-- <<http://server.c-otto.de/xmonad/NotebookMiddle.png>>
--
newtype IncColumnN = IncColumnN Int deriving Typeable
instance Message IncColumnN

data ToggleMiddle = ToggleMiddle deriving (Read, Show, Typeable)
instance Message ToggleMiddle

data ToggleSide = ToggleSide deriving (Read, Show, Typeable)
instance Message ToggleSide

data ToggleStackDir = ToggleStackDir deriving (Read, Show, Typeable)
instance Message ToggleStackDir

-- | Arguments are nmaster, delta, fraction, mirror fraction
data Notebook a = Notebook{ minResWidth :: !Int, notebookMiddle :: !Bool, notebookSide :: !Bool, stackDirection :: !Bool, notebookMaster :: !Int, notebookColumn :: !Int, notebookDelta :: !Rational, notebookFrac :: !Rational, notebookMirrorFrac :: !Rational}
    deriving (Show,Read)

instance LayoutClass Notebook a where
    pureLayout (Notebook res mid s dir n c _ f mf) r    = doL res mid s dir n c f mf r
    handleMessage l m =
        return $ msum [fmap resize         (fromMessage m)
                      ,fmap mresize        (fromMessage m)
                      ,fmap incmastern     (fromMessage m)
                      ,fmap inccolumnn     (fromMessage m)
                      ,fmap togglemiddle   (fromMessage m)
                      ,fmap toggleside     (fromMessage m)
                      ,fmap togglestackdir (fromMessage m)]
            where resize Shrink = l { notebookFrac = min 3 $ f+d }
                  resize Expand = l { notebookFrac = max 1 $ f-d }
                  mresize MirrorShrink = l { notebookMirrorFrac = max (-0.5) $ mf-d }
                  mresize MirrorExpand = l { notebookMirrorFrac = min 1 $ mf+d }
                  incmastern (IncColumnN x) = l { notebookMaster = max 1 $ min c (n+x) }
                  inccolumnn (IncMasterN x) = l { notebookColumn = max n (c+x) }
                  togglemiddle ToggleMiddle = l { notebookMiddle = not mid}
                  toggleside ToggleSide = l { notebookSide = not s}
                  togglestackdir ToggleStackDir = l { stackDirection  = not dir}
                  mid = notebookMiddle l
                  s = notebookSide l
                  n = notebookMaster l
                  c = notebookColumn l
                  d = notebookDelta l
                  f = notebookFrac l
                  mf = notebookMirrorFrac l
                  dir = stackDirection l
    description _ = "Notebook"

doL :: Int -> Bool -> Bool -> Bool -> Int -> Int -> Rational -> Rational -> Rectangle -> W.Stack a -> [(a, Rectangle)]
doL res m s dir n c f mf r st
    | rect_width r >= fromIntegral res = zip sti (newWide m s dir n c nwin f mf r)
    | c > 1  = zip stackList (tileSlim m f1 mf r n c nwin)
    | c == 0 = zip sti (splitHorizontally nwin r)
    | m      = zip sti (r1 : splitVertically (nwin-c) r2)
    | not m  = zip sti (q1 : splitHorizontally (nwin-c) q2)
    | otherwise = zip sti (splitHorizontally nwin r)
            where sti = W.integrate st
                  nwin = length sti
                --   ncol = if n <= c
                        --  then c-n
                        --  else 0
                  (q1, q2) = splitVerticallyBy (if f1<0 then 1+2*f1 else f1) r
                  (r1, r2) = splitHorizontallyBy (if f1<0 then 1+2*f1 else f1) r
                  f1 = (1/f)*(4/3)
                  foc = W.focus st
                  u = W.up st
                  d = W.down st
                  stackList
                     | length u < c && length u > n = u1 ++ [foc] ++ u2 ++ d
                     | length u < n = u3 ++ [foc] ++ u4 ++ d
                     | m && length u > c = u5 ++ [foc] ++ u6 ++ d
                     | otherwise = sti
                          where (u1, u2) = splitAt n (reverse u)
                                (u3, u4) = splitAt 0 (reverse u)
                                (u5, u6) = splitAt c (reverse u)

tileSlim :: Bool -> Rational -> Rational -> Rectangle -> Int -> Int -> Int -> [Rectangle]
tileSlim middle f mf r nmain nmaster n
 | n <= nmain = rep nmain r
 | n <= nmaster  = rep nmain r1 ++ rep (nmaster-nmain) r2
 | not middle && (n-nmaster-nmain) <= 2 = rep nmain r11 ++ rep (nmaster-nmain) r2 ++ splitHorizontally (n-nmaster-nmain) r12
 | not middle = rep nmain r11 ++ rep (nmaster-nmain) r21 ++ splitHorizontally (n-nmaster-nmain) q2
 | middle = rep nmain r1 ++ rep (nmaster-nmain) r21 ++ rep (n-nmaster-nmain) r22
 | otherwise = splitHorizontally n r
 where q2 = snd $ splitVerticallyBy (if mf<0 then 1+2*mf else mf) r
       (r1, r2) = splitHorizontallyBy (if f<0 then 1+2*f else f) r
       (r11, r12) = splitVerticallyBy (if mf<0 then 1+2*mf else mf) r1
       (r21, r22) = splitVerticallyBy (if mf<0 then 1+2*mf else mf) r2

newWide ::Bool -> Bool -> Bool -> Int -> Int -> Int -> Rational -> Rational -> Rectangle -> [Rectangle]
newWide m s d n c nwin f mf r
    | nwin <= ncol + nmain = map fst listCols
    | otherwise = listWithStack
    where startPoint
            | m && s     && even (nmain+ncol)               = toInteger (rect_x r) + (floor (nmain%2) * fromIntegral width) + (ceiling (ncol%2) * fromIntegral colWidth)
            | m && s                                        = toInteger (rect_x r) + (floor (nmain%2) * fromIntegral width) + (floor (ncol%2) * fromIntegral colWidth)
            | m && not s && even (nmain+ncol) && odd nmain  = toInteger (rect_x r) + (floor (nmain%2) * fromIntegral width) + (ceiling ((ncol-1)%2) * fromIntegral colWidth)
            | m && not s                                    = toInteger (rect_x r) + (floor ((nmain-1)%2) * fromIntegral width) + (ceiling (ncol%2) * fromIntegral colWidth)
            | not m && s                                    = toInteger (rect_x r)
            | not m && not s                                = toInteger (rect_x r) + toInteger (rect_width r) - toInteger width
            | otherwise                                     = toInteger width

          width
            | c <= 1 = rect_width r
            | ncol == 0 = floor (rect_width r % fromIntegral nmain)
            | otherwise = 2 * floor (toRational (rect_width r) / (toRational ncol + (f * toRational nmain)))
          colWidth
            | c <= 1 = 0
            | ncol == 0 = floor (toRational width/2)
            | otherwise = floor (toRational (toRational (rect_width r) - (toRational width*toRational nmain)) / toRational ncol)

          minWidth = colWidth * nstack

          nmain
            | n < nwin = n
            | n > c = c
            | otherwise = nwin
          ncol
            | c >= nwin = nwin - nmain
            | c < n = 0
            | otherwise = c - n
          nstack = nwin - nmain - ncol

          listCols
            | m     && s     = splitMiddleMaster (-1) (fromInteger startPoint) (fromIntegral width) colWidth 1 nmain ncol f r
            | not m && s     = splitLeftMaster (fromInteger startPoint) (fromIntegral width) colWidth 1 nmain ncol f r
            | m     && not s = splitRightMiddleMaster 1 (fromInteger startPoint) (fromIntegral width) colWidth 1 nmain ncol f r
            | otherwise = splitRightMaster (fromInteger startPoint) (fromIntegral width) colWidth 1 nmain ncol f r

          listAll
            | d     = splitColumns (sortByXLocation listCols) (fromIntegral minWidth - 10) initStackRect mf d
            | otherwise = splitColumns (sortByRevXLocation listCols) (fromIntegral minWidth - 10) initStackRect mf d

          listWithStack
            | d     = map fst (sortByIndex $ init listAll) ++ splitHorizontally nstack (fst $ last listAll)
            | otherwise = map fst (sortByIndex $ init listAll) ++ reverse (splitHorizontally nstack (fst $ last listAll))

          initYPos = floor $ fromIntegral (rect_height r) * mf
          initHeight = rect_height r - fromIntegral initYPos
          initStackRect = Rectangle (rect_x r) (initYPos + rect_y r) 0 initHeight


splitLeftMaster :: Int -> Int -> Int -> Int -> Int -> Int -> Rational -> Rectangle -> [(Rectangle, Int)]
splitLeftMaster xpos width colWidth count main colu f rect
    | count <= main        = (mainRect, countN) : splitLeftMaster xposN width colWidth countN main colu f rect
    | count <= colu + main = (coluRect, countN) : splitLeftMaster xposNN width colWidth countN main colu f rect
    | otherwise = []
    where mainRect = Rectangle  (fromIntegral xpos)  (rect_y rect) (fromIntegral width) (rect_height rect)
          coluRect = Rectangle  (fromIntegral xpos) (rect_y rect) (fromIntegral colWidth) (rect_height rect)
          countN = count + 1
          xposN = xpos + width
          xposNN = xpos + colWidth

splitRightMaster :: Int -> Int -> Int -> Int -> Int -> Int -> Rational -> Rectangle -> [(Rectangle, Int)]
splitRightMaster xpos width colWidth count main colu f rect
    | count < main         = (mainRect, countN) : splitRightMaster xposN width colWidth countN main colu f rect
    | count == main        = (mainRect, countN) : splitRightMaster xposNN width colWidth countN main colu f rect
    | count <= colu + main = (coluRect, countN) : splitRightMaster xposNN width colWidth countN main colu f rect
    | otherwise = []
    where mainRect = Rectangle  (fromIntegral xpos)  (rect_y rect) (fromIntegral width) (rect_height rect)
          coluRect = Rectangle  (fromIntegral xpos) (rect_y rect) (fromIntegral colWidth) (rect_height rect)
          countN = count + 1
          xposN = xpos + (-1 * width)
          xposNN = xpos + (-1 * colWidth)

splitMiddleMaster :: Int -> Int -> Int -> Int -> Int -> Int -> Int-> Rational -> Rectangle -> [(Rectangle, Int)]
splitMiddleMaster sign xpos width colWidth count main colu f rect
    | count <  main        = (mainRect, countN) : splitMiddleMaster (-sign) xposN width colWidth countN main colu f rect
    | count == main        = (mainRect, countN) : splitMiddleMaster (-sign) xposNN width colWidth countN main colu f rect
    | count <= colu + main = (coluRect, countN) : splitMiddleMaster (-sign) xposNNN width colWidth countN main colu f rect
    | otherwise = []
    where mainRect = Rectangle  (fromIntegral xpos)  (rect_y rect) (fromIntegral width) (rect_height rect)
          coluRect = Rectangle  (fromIntegral xpos) (rect_y rect) (fromIntegral colWidth) (rect_height rect)
          countN = count + 1
          xposN = xpos + (sign * width * count)
          xposNN = if even main
              then xpos + (sign * ((colWidth * (count - main)) + (width * main)))
              else xpos + (sign * ((colWidth * ((1+count) - main)) + (width * (main-1))))
          xposNNN = xpos + (sign * ((colWidth * (count - main)) + (width * main)))

splitRightMiddleMaster :: Int -> Int -> Int -> Int -> Int -> Int -> Int-> Rational -> Rectangle -> [(Rectangle, Int)]
splitRightMiddleMaster sign xpos width colWidth count main colu f rect
    | count <  main        = (mainRect, countN) : splitRightMiddleMaster (-sign) xposN width colWidth countN main colu f rect
    | count <= main        = (mainRect, countN) : splitRightMiddleMaster (-sign) xposNN width colWidth countN main colu f rect
    | count <= colu + main = (coluRect, countN) : splitRightMiddleMaster (-sign) xposNNN width colWidth countN main colu f rect
    | otherwise = []
    where mainRect = Rectangle  (fromIntegral xpos)  (rect_y rect) (fromIntegral width) (rect_height rect)
          coluRect = Rectangle  (fromIntegral xpos) (rect_y rect) (fromIntegral colWidth) (rect_height rect)
          countN = count + 1
          xposN = xpos + (sign * width * count)
          xposNN = if even main
              then xpos + (sign * ((colWidth * ((1+count) - main)) + (width * (main-1))))
              else xpos + (sign * ((colWidth * (count - main)) + (width * main)))
          xposNNN = xpos + (sign * ((colWidth * (count - main)) + (width * main)))

splitColumns :: [(Rectangle, Int)] -> Int -> Rectangle -> Rational -> Bool -> [(Rectangle, Int)]
splitColumns list minWidth stackRect mf d
  | not (null list) && rect_width stackRect <= fromIntegral minWidth = (masterRect, index) : splitColumns listN minWidth stackRectN mf d
  | not (null list) = (rect, index) : splitColumns listN minWidth stackRect mf d
  | otherwise = [(stackRect, 0)]
  where rect = fst $ head list
        index = snd $ head list
        listN = tail list
        (masterRect, stackRectAdd) = splitVerticallyBy mf rect
        stackRectN
          | d     = rectangleDiff stackRect stackRectAdd
          | otherwise = rectangleDiff stackRectAdd stackRect


sortByXLocation :: [(Rectangle, Int)] -> [(Rectangle, Int)]
sortByXLocation = sortOn (rect_x . fst)

sortByRevXLocation :: [(Rectangle, Int)] -> [(Rectangle, Int)]
sortByRevXLocation = sortOn (Data.Ord.Down . (rect_x . fst))

sortByIndex :: [(Rectangle , Int)] -> [(Rectangle , Int)]
sortByIndex = sortOn snd

rectangleDiff :: Rectangle -> Rectangle -> Rectangle
rectangleDiff firstRect secondRect =
    Rectangle (rect_x firstRect) (rect_y firstRect) (rect_width firstRect + rect_width secondRect) (rect_height firstRect)

rep :: Int -> rect -> [rect]
rep n x =
  if n == 1
    then [x]
    else x : rep (n-1) x
