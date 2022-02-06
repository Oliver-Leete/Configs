-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.CycleWS
-- Description :  Cycle through workspaces.
-- Copyright   :  (c) Joachim Breitner <mail@joachim-breitner.de>,
--                    Nelson Elhage <nelhage@mit.edu> (`toggleWS' function)
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Joachim Breitner <mail@joachim-breitner.de>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Provides bindings to cycle forward or backward through the list of
-- workspaces, to move windows between workspaces, and to cycle
-- between screens.  More general combinators provide ways to cycle
-- through workspaces in various orders, to only cycle through some
-- subset of workspaces, and to cycle by more than one workspace at a
-- time.
--
-- Note that this module now subsumes the functionality of the former
-- @XMonad.Actions.RotView@.  Former users of @rotView@ can simply replace
-- @rotView True@ with @moveTo Next (Not emptyWS)@, and so on.
--
-- If you want to exactly replicate the action of @rotView@ (cycling
-- through workspace in order lexicographically by tag, instead of in
-- the order specified in the config), it can be implemented as:
--
-- > rotView b  = do t <- findWorkspace getSortByTag (bToDir b) (Not emptyWS) 1
-- >                 windows . greedyView $ t
-- >   where bToDir True  = Next
-- >         bToDir False = Prev
--
-----------------------------------------------------------------------------

module XMonad.Actions.CycleWSLocal (
                                toggleWS'
                              , shiftToggleWS'
                             ) where

import XMonad.Prelude (find)
import XMonad hiding (workspaces)
import qualified XMonad.Hooks.WorkspaceHistory as WH
import XMonad.StackSet hiding (filter)

-- | Toggle to the previous workspace while excluding some workspaces.
--
-- > -- Ignore the scratchpad workspace while toggling:
-- > ("M-b", toggleWS' ["NSP"])
toggleWS' :: [WorkspaceId] -> X ()
toggleWS' skips = lastViewedHiddenExcept skips >>= flip whenJust (windows . greedyView)

-- | Same as toggleWS' but sends the focused window instead of moving workspace
shiftToggleWS' :: [WorkspaceId] -> X ()
shiftToggleWS' skips = lastViewedHiddenExcept skips >>= flip whenJust (windows . shift)

-- | List difference ('\\') for workspaces and tags. Removes workspaces
-- matching listed tags from the given workspace list.
skipTags :: (Eq i) => [Workspace i l a] -> [i] -> [Workspace i l a]
skipTags wss ids = filter ((`notElem` ids) . tag) wss

-- | Ignoring the skips, find the best candidate for the last viewed hidden
-- workspace.
lastViewedHiddenExcept :: [WorkspaceId] -> X (Maybe WorkspaceId)
lastViewedHiddenExcept skips = do
    hs2 <- gets $ map tag . flip skipTags skips . (map workspace . visible) . windowset
    hs <- gets $ map tag . flip skipTags skips . hidden . windowset
    let hs3 = hs2 ++ hs
    choose hs3 . find (`elem` hs3) <$> WH.workspaceHistory
    where choose []    _           = Nothing
          choose (h:_) Nothing     = Just h
          choose _     vh@(Just _) = vh
