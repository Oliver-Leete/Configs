{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.TwoPanePersistent
-- Description :  "XMonad.Layout.TwoPane" with a persistent stack window.
-- Copyright   :  (c) Chayanon Wichitrnithed
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Chayanon Wichitrnithed <namowi@gatech.edu>
-- Stability   :  unstable
-- Portability :  unportable
--
-- This layout is the same as "XMonad.Layout.TwoPane" except that it keeps track of the slave window
-- that is alongside the master pane. In other words, it prevents the slave pane
-- from changing after the focus goes back to the master pane.

-----------------------------------------------------------------------------


module XMonad.Layout.TwoPanePersistentLocal
  (
    -- * Usage
    -- $usage
  TwoPanePersistent(..)
  ) where

import XMonad.StackSet (focus, up, down, Stack, Stack(..))
import XMonad hiding (focus)

-- $usage
-- Import the module in @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.TwoPanePersistent
--
-- Then add the layout to the @layoutHook@:
--
-- > myLayout = TwoPanePersistent Nothing (3/100) (1/2) ||| Full ||| etc..
-- > main = xmonad def { layoutHook = myLayout }


data TwoPanePersistent a = TwoPanePersistent
  { slaveWin :: Maybe a  -- ^ slave window; if 'Nothing' or not in the current workspace,
                         -- the window below the master will go into the slave pane
  , dFrac :: Rational -- ^ shrink/expand size
  , mFrac :: Rational -- ^ initial master size
  } deriving (Show, Read)


instance (Show a, Eq a) => LayoutClass TwoPanePersistent a where
  doLayout l r s =
    case reverse (up s) of
      -- master is focused
      []         -> return $ focusedMaster l s r

      -- slave is focused
      (master:_) -> return $ focusedSlave l s r master


  pureMessage (TwoPanePersistent w delta split) x =
    case fromMessage x of
      Just Shrink -> Just (TwoPanePersistent w delta (split - delta))
      Just Expand -> Just (TwoPanePersistent w delta (split + delta))
      _ -> Nothing

  description _ = "TwoPanePersistent"


----------------------------------------------------------------------------------------

focusedMaster :: (Eq a) => TwoPanePersistent a -> Stack a -> Rectangle
              -> ( [(a, Rectangle)], Maybe (TwoPanePersistent a) )
focusedMaster (TwoPanePersistent w delta split) s r =
  let (left, right) = modYsplitHorisontallyBy split r in
      case down s of
        -- there exist windows below the master
        (next:_) -> let nextSlave = ( [(focus s, left), (next, right)]
                                    , Just $ TwoPanePersistent (Just next) delta split )
                    in case w of
                      -- if retains state, preserve the layout
                      Just win -> if win `elem` down s && (focus s /= win)
                                  then ( [(focus s, left), (win, right)]
                                       , Just $ TwoPanePersistent w delta split )
                                  else nextSlave
                      -- if no previous state, default to the next slave window
                      Nothing -> nextSlave


        -- the master is the only window
        []       -> ( [(focus s, modY r r)]
                    , Just $ TwoPanePersistent Nothing delta split )


focusedSlave :: TwoPanePersistent a -> Stack a -> Rectangle -> a
             -> ( [(a, Rectangle)], Maybe (TwoPanePersistent a) )
focusedSlave (TwoPanePersistent _ delta split) s r m =
  ( [(m, left), (focus s, right)]
  , Just $ TwoPanePersistent (Just $ focus s) delta split )
  where (left, right) = modYsplitHorisontallyBy split r

modYsplitHorisontallyBy ::  Rational -> Rectangle -> (Rectangle, Rectangle)
modYsplitHorisontallyBy split r =
    (le, ri)
    where (left, right) = splitHorizontallyBy split r
          le = modY r left
          ri = modY r right

modY :: Rectangle -> Rectangle -> Rectangle
modY (Rectangle bx _ bw _) (Rectangle sx sy sw sh) =
    Rectangle sx (sy + ymoddifier) sw (sh - fromIntegral ymoddifier)
    where   ymoddifier= if toInteger (8 + sx) < toInteger bx + xmobarWidth
                        then 31
                        else 0
            xmobarWidth = if bw > 1920
                            then 960
                            else 1280
