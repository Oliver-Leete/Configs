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
    | nTotColLimit == 0 = map colModifier (splitHorizontally nwin r)
    | nwin <= ncol + nmain = map (colModifier . fst) listCols
    | otherwise = listWithStack
    where
            width
                | nmain == 0 = floor (rect_width r % fromIntegral ncol)
                | ncol + nmain <= 1 = fromIntegral (rect_width r)
                | ncol == 0 = floor (rect_width r % fromIntegral nmain)
                | otherwise = floor ((toRational (rect_width r) / t) * f)
            t = toRational ncol + (f * toRational nmain)
            colWidth
                | nmain == 0 = fromIntegral width
                | ncol == 0 = floor (toRational width/2)
                | otherwise = floor $ toRational (fromIntegral (rect_width r) - width * nmain) / toRational ncol
            minWidth = floor (fromIntegral colWidth * sf * fromIntegral nstack)::Int

            nmain = minimum [nMainLimit,nTotColLimit,nwin]
            ncol = min (max (nTotColLimit - nmain) 0) (nwin - nmain)
            nstack = nwin - nmain - ncol

            startOffset
                | even (nmain + ncol) = ceiling (ncol%2) * colWidth
                | otherwise = floor (ncol%2) * colWidth

            colLister True = splitMiddleMaster (-1) (fromIntegral (rect_x r) + floor (nmain%2) * width + startOffset)
            colLister False = splitLeftMaster (fromIntegral $ rect_x r)

            listCols = colLister m (fromIntegral width) colWidth 1 nmain ncol f r

            listAll = splitColumns (sortByXLocation d listCols) (fromIntegral minWidth - 10) initStackRect mf d s r

            splitStack True False = splitHorizontally nstack (fst $ last listAll)
            splitStack False False = reverse $ splitStack True False
            splitStack x True = map (reflectRect r) (splitStack x False)

            colModifier
                | s = (`modY` r) . reflectRect r
                | otherwise = (`modY` r)

            listWithStack = map (colModifier . fst) (sortByIndex $ init listAll) ++ splitStack d s

            initYPos = floor $ fromIntegral (rect_height r) * mf
            initStackRect = Rectangle (rect_x r) (initYPos + rect_y r) 0 (rect_height r - fromIntegral initYPos)

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
    | count <  main        = lister (rectN width) xposN
    | count == main        = lister (rectN width) (xposNN $ even main)
    | count <= colu + main = lister (rectN colWidth) (xposNN True)
    | otherwise = []
    where   lister wid xp = (wid, countN) : splitMiddleMaster (-sign) xp width colWidth countN main colu f rect
            rectN w = Rectangle (fromIntegral xpos)  (rect_y rect) (fromIntegral w) (rect_height rect)
            countN = count + 1
            xposN = xpos + (sign * width * count)
            xposNN True =  xpos + (sign * ((colWidth * (count - main)) + (width * main)))
            xposNN False =  xpos + (sign * ((colWidth * ((1+count) - main)) + (width * (main-1))))

splitColumns :: [(Rectangle, Int)] -> Int -> Rectangle -> Rational -> Bool -> Bool -> Rectangle -> [(Rectangle, Int)]
splitColumns list minWidth stackRect mf d s bigRect
    | null list = [(stackRect, 0)]
    | rect_width stackRect <= fromIntegral minWidth = lister rectN (stackRectFunc d stackRect stackRectAdd)
    | otherwise = lister rect stackRect
    where   lister r sr = (r, index) : splitColumns listNew minWidth sr mf d s bigRect
            rect = fst $ head list
            index = snd $ head list
            listNew = tail list
            (rectN, stackRectAdd) = splitVerticallyBy mf rect
            stackRectFunc True = rectangleDiff
            stackRectFunc False =  flip rectangleDiff

modY :: Rectangle -> Rectangle -> Rectangle
modY (Rectangle sx sy sw sh) (Rectangle bx _ _ _)=
    Rectangle sx (sy + ymoddifier) sw (sh - fromIntegral ymoddifier)
    where   ymoddifier= if toInteger (8 + sx) < toInteger bx + 960
                        then 31
                        else 0

sortByXLocation :: Bool -> [(Rectangle, b)] -> [(Rectangle, b)]
sortByXLocation True = sortOn (rect_x . fst)
sortByXLocation False = sortOn (Data.Ord.Down . (rect_x . fst))

sortByIndex :: [(Rectangle , Int)] -> [(Rectangle , Int)]
sortByIndex = sortOn snd

rectangleDiff :: Rectangle -> Rectangle -> Rectangle
rectangleDiff firstRect secondRect =
    Rectangle (rect_x firstRect) (rect_y firstRect) (rect_width firstRect + rect_width secondRect) (rect_height firstRect)

reflectRect :: Rectangle -> Rectangle -> Rectangle
reflectRect (Rectangle sx _ sw _) (Rectangle bx by bw bh) =
  Rectangle (2*sx + fi sw - bx - fi bw) by bw bh
