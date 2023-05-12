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
import           XMonad.Prelude              (fi)
import qualified XMonad.StackSet             as W

import           Data.Ratio                  ((%))

import           Control.Monad               (msum)
import           Data.List                   (sortOn)
import           Graphics.X11.Xlib           (Dimension, Rectangle (..))
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
                    sresize SShrink = l { notebookStackFrac = max 0.2 $ sf-d }
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
newWide m s d nMainLimit nTotColLimit nwin f mf sf r
    | s && nTotColLimit == 0 = map colModifier (splitHorizontally nwin r)
    | nTotColLimit == 0 = map colModifier (splitHorizontally nwin r)
    | s && nwin <= ncol + nmain = map (colModifier . fst) listCols
    | nwin <= ncol + nmain = map (colModifier . fst) listCols
    | otherwise = listWithStack
    where
            width = widthFinder nMainLimit nTotColLimit nmain ncol f r
            colWidth = colWidthFinder nMainLimit nTotColLimit nmain ncol width r
            minWidth = floor (fromIntegral colWidth * sf * fromIntegral nstack)::Int

            nmain = minimum [nMainLimit,nTotColLimit,nwin]
            ncol
                | nTotColLimit >= nwin = nwin - nmain
                | nTotColLimit < nMainLimit = 0
                | otherwise = nTotColLimit - nMainLimit
            nstack = nwin - nmain - ncol

            startOffset
                | even nTotColLimit = ceiling (ncol%2) * colWidth
                | otherwise = floor (ncol%2) * colWidth

            colLister
                | m     = splitMiddleMaster (-1) (fromIntegral (rect_x r) + floor (nmain%2) * width + startOffset)
                | otherwise = splitLeftMaster (fromIntegral $ rect_x r)

            listCols = colLister (fromIntegral width) colWidth 1 nmain ncol f r

            (colSorter, splitStackRMaybe)
                | d = (sortByXLocation, splitStack)
                | otherwise = (sortByRevXLocation, reverse splitStack)

            listAll = splitColumns (colSorter listCols) (fromIntegral minWidth - 10) initStackRect mf d s r

            splitStack = splitHorizontally nstack (fst $ last listAll)

            colModifier
                | s = (`modY` r) . reflectRect r
                | otherwise = (`modY` r)

            listWithStack
                | s = map (colModifier . fst) (sortByIndex $ init listAll) ++ map (reflectRect r) splitStackRMaybe
                | otherwise = map (colModifier . fst) (sortByIndex $ init listAll) ++ splitStackRMaybe

            initYPos = floor $ fromIntegral (rect_height r) * mf
            initHeight = rect_height r - fromIntegral initYPos
            initStackRect = Rectangle (rect_x r) (initYPos + rect_y r) 0 initHeight


widthFinder :: Int -> Int -> Int -> Int -> Rational -> Rectangle -> Int
widthFinder n c nmain ncol f r
    | n == 0 = floor (rect_width r % fromIntegral ncol)
    | c <= 1 = fromIntegral (rect_width r)
    | ncol == 0 = floor (rect_width r % fromIntegral nmain)
    | otherwise = floor ((toRational (rect_width r) / t) * f)
    where t = toRational ncol + (f * toRational nmain)

colWidthFinder :: Int -> Int -> Int -> Int -> Int -> Rectangle -> Int
colWidthFinder n c nmain ncol width (Rectangle _ _ rw _)
    | n == 0 = fromIntegral width
    | c <= 1 = 0
    | ncol == 0 = floor (toRational width/2)
    | otherwise = floor $ toRational (fromIntegral rw - width * nmain) / toRational ncol

splitLeftMaster :: Int -> Int -> Int -> Int -> Int -> Int -> Rational -> Rectangle -> [(Rectangle, Int)]
splitLeftMaster xpos width colWidth count main colu f rect
    | count <= main        = lister width
    | count <= colu + main = lister colWidth
    | otherwise = []
    where   lister w = (rectN w, countN) : splitLeftMaster (xposN w) width colWidth countN main colu f rect
            rectN w = Rectangle (fromIntegral xpos) (rect_y rect) (fromIntegral w) (rect_height rect)
            countN = count + 1
            xposN w = xpos + w

splitMiddleMaster :: Int -> Int -> Int -> Int -> Int -> Int -> Int-> Rational -> Rectangle -> [(Rectangle, Int)]
splitMiddleMaster sign xpos width colWidth count main colu f rect
    | count <  main        = (rectN width, countN) : splitMiddleMaster (-sign) xposN width colWidth countN main colu f rect
    | count == main        = (rectN width, countN) : splitMiddleMaster (-sign) xposNN width colWidth countN main colu f rect
    | count <= colu + main = (rectN colWidth, countN) : splitMiddleMaster (-sign) xposNNN width colWidth countN main colu f rect
    | otherwise = []
    where   rectN w = Rectangle (fromIntegral xpos)  (rect_y rect) (fromIntegral w) (rect_height rect)
            countN = count + 1
            xposN = xpos + (sign * width * count)
            xposNN = if even main
                then xpos + (sign * ((colWidth * (count - main)) + (width * main)))
                else xpos + (sign * ((colWidth * ((1+count) - main)) + (width * (main-1))))
            xposNNN = xpos + (sign * ((colWidth * (count - main)) + (width * main)))

splitColumns :: [(Rectangle, Int)] -> Int -> Rectangle -> Rational -> Bool -> Bool -> Rectangle -> [(Rectangle, Int)]
splitColumns list minWidth stackRect mf d s bigRect
    | not (null list) && rect_width stackRect <= fromIntegral minWidth = appender rectN (stackRectFunc stackRect stackRectAdd)
    | not (null list) = appender rect stackRect
    | otherwise = [(stackRect, 0)]
    where   appender r sr = (r, index) : splitColumns listNew minWidth sr mf d s bigRect
            rect = fst $ head list
            index = snd $ head list
            listNew = tail list
            (rectN, stackRectAdd) = splitVerticallyBy mf rect
            stackRectFunc
                | d = rectangleDiff
                | otherwise = flip rectangleDiff

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

reflectRect :: Rectangle -> Rectangle -> Rectangle
reflectRect (Rectangle sx _ sw _) (Rectangle bx by bw bh) =
  Rectangle (2*sx + fi sw - bx - fi bw) by bw bh
