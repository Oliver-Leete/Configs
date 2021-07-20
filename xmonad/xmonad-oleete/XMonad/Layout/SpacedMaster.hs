{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, PatternGuards, TypeSynonymInstances, DeriveDataTypeable, LambdaCase, MultiWayIf #-}

module XMonad.Layout.SpacedMaster (
    SpacedMaster(..)
   )where

import XMonad.Core
import Graphics.X11 (Rectangle(..))
import qualified XMonad.StackSet as W
import XMonad(splitVertically, splitHorizontallyBy)
import Control.Monad

data Resize     = Shrink | Expand
data IncMasterN = IncMasterN !Int

instance Message Resize
instance Message IncMasterN

data SpacedMaster a = SpacedMaster { tallNMaster :: !Int
                   , tallRatioIncrement :: !Rational
                   , tallRatio :: !Rational
                   }
                deriving (Show, Read)

instance LayoutClass SpacedMaster a where
    pureLayout (SpacedMaster nmaster _ frac) r s = zip ws rs
      where ws = W.integrate s
            rs = tileSpace frac r nmaster (length ws)

    pureMessage (SpacedMaster nmaster delta frac) m =
            msum [fmap resize     (fromMessage m)
                 ,fmap incmastern (fromMessage m)]

      where resize Shrink             = SpacedMaster nmaster delta (max 0 $ frac-delta)
            resize Expand             = SpacedMaster nmaster delta (min 1 $ frac+delta)
            incmastern (IncMasterN d) = SpacedMaster (max 0 (nmaster+d)) delta frac

    description _ = "SpacedMaster"

tileSpace frac r nmaster n
      | n == 1 = [r]
      | otherwise = splitVertically r1 nmaster ++ splitVerticallySpaced r2 (n-nmaster)
      where (r1, r2) = splitHorizontallyBy frac r

splitVerticallySpaced (Rectangle sx sy sw sh) n
    if n > 1
    then (Rectangle sx sy sw hight) ++ splitVerticallySpaced (Rectangle sx (sy + hight)) n-1

