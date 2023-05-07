{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
                              ToggleStackDir(..),
                              SResize(..)
                             ) where

import           XMonad                      (IncMasterN (IncMasterN),
                                              LayoutClass (description, handleMessage, pureLayout),
                                              Resize (Expand, Shrink), Typeable,
                                              fromMessage, splitHorizontally,
                                              splitVerticallyBy)
import           XMonad.Layout.ResizableTile (MirrorResize (..))
import qualified XMonad.StackSet             as W

import           Data.Ratio                  ((%))

import           Control.Monad               (msum)
import           Data.List                   (sortOn)
import           Graphics.X11.Xlib           (Rectangle (..))
-- import qualified Basement.Compat.Base as GHC.Int
import qualified Data.Ord
import           XMonad.Core                 (Message)
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
data Notebook a = Notebook{ notebookMiddle :: !Bool, notebookSide :: !Bool, stackDirection :: !Bool, notebookMaster :: !Int, notebookColumn :: !Int, notebookDelta :: !Rational, notebookMirrorDelta :: !Rational, notebookFrac :: !Rational, notebookMirrorFrac :: !Rational, notebookStackFrac :: !Rational }
    deriving (Show,Read)

data SResize     = SShrink | SExpand
instance Message SResize

instance LayoutClass Notebook a where
    pureLayout (Notebook mid s dir n c _ _ f mf sf) r    = doL mid s dir n c f mf sf r
    handleMessage l m =
        return $ msum   [fmap resize         (fromMessage m)
                        ,fmap mresize        (fromMessage m)
                        ,fmap sresize        (fromMessage m)
                        ,fmap incmastern     (fromMessage m)
                        ,fmap inccolumnn     (fromMessage m)
                        ,fmap togglemiddle   (fromMessage m)
                        ,fmap toggleside     (fromMessage m)
                        ,fmap togglestackdir (fromMessage m)]
            where   resize Shrink = l { notebookFrac = max 1 $ f-d }
                    resize Expand = l { notebookFrac = min 10 $ f+d }
                    mresize MirrorShrink = l { notebookMirrorFrac = max (1/10) $ mf-dm }
                    mresize MirrorExpand = l { notebookMirrorFrac = min (9/10) $ mf+dm }
                    sresize SShrink = l { notebookStackFrac = max 0.3 $ sf-d }
                    sresize SExpand = l { notebookStackFrac = min 10 $ sf+d }
                    incmastern (IncColumnN x) = l { notebookMaster = max 0 $ min c (n+x) }
                    inccolumnn (IncMasterN x) = l { notebookColumn = max n (c+x) }
                    togglemiddle ToggleMiddle = l { notebookMiddle = not mid}
                    toggleside ToggleSide = l { notebookSide = not s}
                    togglestackdir ToggleStackDir = l { stackDirection  = not dir}
                    mid = notebookMiddle l
                    s = notebookSide l
                    n = notebookMaster l
                    c = notebookColumn l
                    d = notebookDelta l
                    dm = notebookMirrorDelta l
                    f = notebookFrac l
                    mf = notebookMirrorFrac l
                    sf = notebookStackFrac l
                    dir = stackDirection l
    description _ = "Notebook"

doL :: Bool -> Bool -> Bool -> Int -> Int -> Rational -> Rational -> Rational -> Rectangle -> W.Stack a -> [(a, Rectangle)]
doL m s dir n c f mf sf r st = zip sti (newWide m s dir n c nwin f mf sf r)
        where   sti = W.integrate st
                nwin = length sti

newWide ::Bool -> Bool -> Bool -> Int -> Int -> Int -> Rational -> Rational -> Rational -> Rectangle -> [Rectangle]
newWide m s d n c nwin f mf sf r
    | c == 0 = map (`modY` r) (splitHorizontally nwin r)
    | nwin <= ncol + nmain = map ((`modY` r) . fst) listCols
    | otherwise = listWithStack
    where   startPoint
                | m && s     && even (nmain+ncol)               = toInteger (rect_x r) + (floor (nmain%2) * fromIntegral width) + (ceiling (ncol%2) * fromIntegral colWidth)
                | m && s                                        = toInteger (rect_x r) + (floor (nmain%2) * fromIntegral width) + (floor (ncol%2) * fromIntegral colWidth)
                | m && not s && even (nmain+ncol) && odd nmain  = toInteger (rect_x r) + (floor (nmain%2) * fromIntegral width) + (ceiling ((ncol-1)%2) * fromIntegral colWidth)
                | m && not s                                    = toInteger (rect_x r) + (floor ((nmain-1)%2) * fromIntegral width) + (ceiling (ncol%2) * fromIntegral colWidth)
                | not m && s                                    = toInteger (rect_x r)
                | not m && not s                                = toInteger (rect_x r) + toInteger (rect_width r) - toInteger width
                | otherwise                                     = toInteger width

            width
                | n == 0 = floor (rect_width r % fromIntegral ncol)
                | c <= 1 = rect_width r
                | ncol == 0 = floor (rect_width r % fromIntegral nmain)
                | otherwise = floor ((toRational (rect_width r) / t) * f)
                where t = toRational ncol + (f * toRational nmain)
            colWidth
                | n == 0 = fromIntegral width
                | c <= 1 = 0
                | ncol == 0 = floor (toRational width/2)
                | otherwise = floor (toRational (toRational (rect_width r) - (toRational width*toRational nmain)) / toRational ncol)
            minWidth = floor (fromIntegral colWidth * sf * fromIntegral nstack)

            nmain
                | n == 0 = 0
                | n < nwin = n
                | n > c = c
                | otherwise = nwin
            ncol
                | c >= nwin = nwin - nmain
                | c < n = 0
                | otherwise = c - n
            nstack = nwin - nmain - ncol

            colLister
                | m     && s     = splitMiddleMaster (-1)
                | not m && s     = splitLeftMaster
                | m     && not s = splitRightMiddleMaster 1
                | otherwise = splitRightMaster

            listCols = colLister (fromInteger startPoint) (fromIntegral width) colWidth 1 nmain ncol f r

            colSorter
                | d = sortByXLocation
                | otherwise = sortByRevXLocation

            listAll = splitColumns (colSorter listCols) (fromIntegral minWidth - 10) initStackRect mf d r

            splitStack = splitHorizontally nstack (fst $ last listAll)

            splitStackRMaybe
                | d = splitStack
                | otherwise = reverse splitStack

            listWithStack = map fst (sortByIndex $ init listAll) ++ splitStackRMaybe

            initYPos = floor $ fromIntegral (rect_height r) * mf
            initHeight = rect_height r - fromIntegral initYPos
            initStackRect = Rectangle (rect_x r) (initYPos + rect_y r) 0 initHeight


splitLeftMaster :: Int -> Int -> Int -> Int -> Int -> Int -> Rational -> Rectangle -> [(Rectangle, Int)]
splitLeftMaster xpos width colWidth count main colu f rect
    | count <= main        = (mainRect, countN) : splitLeftMaster xposN width colWidth countN main colu f rect
    | count <= colu + main = (coluRect, countN) : splitLeftMaster xposNN width colWidth countN main colu f rect
    | otherwise = []
    where   mainRect = Rectangle  (fromIntegral xpos)  (rect_y rect) (fromIntegral width) (rect_height rect)
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
    where   mainRect = Rectangle  (fromIntegral xpos)  (rect_y rect) (fromIntegral width) (rect_height rect)
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
    where   mainRect = Rectangle  (fromIntegral xpos)  (rect_y rect) (fromIntegral width) (rect_height rect)
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
    where   mainRect = Rectangle  (fromIntegral xpos)  (rect_y rect) (fromIntegral width) (rect_height rect)
            coluRect = Rectangle  (fromIntegral xpos) (rect_y rect) (fromIntegral colWidth) (rect_height rect)
            countN = count + 1
            xposN = xpos + (sign * width * count)
            xposNN = if even main
                then xpos + (sign * ((colWidth * ((1+count) - main)) + (width * (main-1))))
                else xpos + (sign * ((colWidth * (count - main)) + (width * main)))
            xposNNN = xpos + (sign * ((colWidth * (count - main)) + (width * main)))

splitColumns :: [(Rectangle, Int)] -> Int -> Rectangle -> Rational -> Bool -> Rectangle -> [(Rectangle, Int)]
splitColumns list minWidth stackRect mf d bigRect
    | not (null list) && rect_width stackRect <= fromIntegral minWidth = (modMastRect, index) : splitColumns listN minWidth stackRectN mf d bigRect
    | not (null list) = (modRect, index) : splitColumns listN minWidth stackRect mf d bigRect
    | otherwise = [(stackRect, 0)]
    where   rect = fst $ head list
            index = snd $ head list
            listN = tail list
            modMastRect = modY masterRect bigRect
            modRect = modY rect bigRect
            (masterRect, stackRectAdd) = splitVerticallyBy mf rect
            stackRectN
                | d     = rectangleDiff stackRect stackRectAdd
                | otherwise = rectangleDiff stackRectAdd stackRect

modY :: Rectangle -> Rectangle -> Rectangle
modY (Rectangle sx sy sw sh) (Rectangle bx _ _ _)=
    Rectangle sx y sw h
    where   ymoddifier= if toInteger (8 + sx) < toInteger bx + 960
                        then 31
                        else 0
            y = sy + ymoddifier
            h = sh - fromIntegral ymoddifier



sortByXLocation :: [(Rectangle, Int)] -> [(Rectangle, Int)]
sortByXLocation = sortOn (rect_x . fst)

sortByRevXLocation :: [(Rectangle, Int)] -> [(Rectangle, Int)]
sortByRevXLocation = sortOn (Data.Ord.Down . (rect_x . fst))

sortByIndex :: [(Rectangle , Int)] -> [(Rectangle , Int)]
sortByIndex = sortOn snd

rectangleDiff :: Rectangle -> Rectangle -> Rectangle
rectangleDiff firstRect secondRect =
    Rectangle (rect_x firstRect) (rect_y firstRect) (rect_width firstRect + rect_width secondRect) (rect_height firstRect)
