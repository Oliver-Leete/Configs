{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}

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
module XMonad.Layout.PaperPersistent (
    -- * Usage
    -- $usage
    PaperPersistent (PaperPersistent),
    IncWindowIndex (..),
) where

import Control.Monad (msum)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ord (clamp)
import XMonad (
    LayoutClass (description, doLayout, handleMessage),
    Message,
    Rectangle (Rectangle),
    Resize (Expand, Shrink),
    Typeable,
    fromMessage,
    modifyWindowSet,
 )
import XMonad.Prelude ((!?))
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

newtype IncWindowIndex = IncWindowIndex Int deriving (Typeable)
instance Message IncWindowIndex

data PaperPersistent a = PaperPersistent
    { leftIndex :: !Int
    , frac :: !Rational
    , fracDelta :: !Rational
    }
    deriving (Read, Show)

instance (Show a, Eq a) => LayoutClass PaperPersistent a where
    handleMessage l m = do
        modifyWindowSet $ W.modify' (noWrapFocus' (maybe 0 toIndexMod (fromMessage m)))
        return $
            msum
                [ fmap incWindowIndex (fromMessage m)
                , fmap resize (fromMessage m)
                ]
      where
        incWindowIndex (IncWindowIndex x) = l{leftIndex = leftIndex l + x}
        toIndexMod (IncWindowIndex x) = x
        resize Shrink = l{frac = max 0.33 $ f - d}
        resize Expand = l{frac = min 1 $ f + d}
        f = l.frac
        d = l.fracDelta

    doLayout (PaperPersistent oldLeftInd f d) sc ws =
        return (mapMaybe dropEmpty $ zip wins (split3 sc f), Just $ PaperPersistent newLeftInd f d)
      where
        fullStack = W.integrate ws
        focWinInd = fromMaybe 0 $ elemIndex ws.focus fullStack
        boundOldLeftInd = clamp (-1, length fullStack - 2) oldLeftInd
        newLeftInd
            | focWinInd > boundOldLeftInd + 2 = focWinInd - 1
            | focWinInd < boundOldLeftInd = focWinInd - 1
            | otherwise = boundOldLeftInd
        inds = [newLeftInd .. (newLeftInd + 2)]
        wins = map (fullStack !?) inds
    description _ = "PaperPersistent"

dropEmpty :: (Maybe a, b) -> Maybe (a, b)
dropEmpty (Just a, b) = Just (a, b)
dropEmpty (Nothing, _) = Nothing

split3 :: Rectangle -> Rational -> [Rectangle]
split3 sc f =
    [ modY sc $ Rectangle (midPos - offset - fromIntegral width) sy width sh
    , modY sc $ Rectangle (midPos - offset) sy width sh
    , modY sc $ Rectangle (midPos + offset) sy width sh
    ]
  where
    (Rectangle sx sy sw sh) = sc
    width = ceiling $ fromIntegral sw * f
    offset = ceiling $ (0.5 :: Double) * fromIntegral width
    midPos = sx + ceiling ((0.5 :: Double) * fromIntegral sw)

modY :: Rectangle -> Rectangle -> Rectangle
modY (Rectangle bx _ bw _) (Rectangle sx sy sw sh) =
    Rectangle sx (sy + ymoddifier) sw (sh - fromIntegral ymoddifier)
  where
    ymoddifier =
        if toInteger (8 + sx) < toInteger bx + xmobarWidth
            then 31
            else 0
    xmobarWidth =
        if bw > 1920
            then 960
            else 1280

-- | A variant of focusDown that doesn't wrap at the end of the stack
noWrapFocus' :: Int -> W.Stack a -> W.Stack a
noWrapFocus' (-1) (W.Stack t (l : ls) rs) = reverseStack (W.Stack l (t : rs) ls)
noWrapFocus' 1 st = reverseStack $ noWrapFocus' (-1) $ reverseStack st
noWrapFocus' _ st = st

reverseStack :: W.Stack a -> W.Stack a
reverseStack (W.Stack t ls rs) = W.Stack t rs ls
