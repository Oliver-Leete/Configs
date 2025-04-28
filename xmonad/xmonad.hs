{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

import Control.Monad ((<=<))
import Data.Function (on)
import Data.List (sortBy)
import qualified Data.Map as M
import Data.Monoid (All)
import Graphics.X11.Types
import qualified Graphics.X11.Xinerama as X11
import qualified Graphics.X11.Xlib as X11
import XMonad hiding ((|||))
import XMonad.Actions.CycleWS (nextScreen, shiftNextScreen)
import XMonad.Actions.CycleWSLocal (shiftToggleWS', toggleWS')
import XMonad.Actions.DynamicProjectsLocal (Project (..), dynamicProjects, runProjectApp1, runProjectApp1Force, runProjectApp2, runProjectApp2Force, runProjectApp3, runProjectApp3Force, runProjectApp4, runProjectApp4Force)
import XMonad.Actions.Navigation2D (Direction2D (D, L, R, U), Navigation2DConfig, windowGo, windowSwap, withNavigation2DConfig)
import XMonad.Actions.PerLayoutKeys (bindByLayout)
import XMonad.Actions.PerWindowKeys (bindFirst)
import XMonad.Actions.ProfilesLocal (Profile (..), ProfileConfig (..), addProfilesWithHistory, addWSToProfilePrompt, excludeWSPP, profileIds, profileLogger, removeWSFromProfilePrompt, shiftProfileWSPrompt, switchProfileWSPrompt, switchToProfile, withNthProfileWorkspace)
import XMonad.Actions.SpawnOn (manageSpawn, spawnOn)
import XMonad.Actions.SwapPromote (masterHistoryHook, swapPromote')
import XMonad.Actions.UpdateFocus (focusUnderPointer)
import XMonad.Actions.UpdatePointer (updatePointer)
import XMonad.Actions.WindowGoLocal as Wgl (raiseNextMaybeCustomFocus2, raiseNextMaybeCustomFocus3, runOrRaiseNext)
import XMonad.Actions.WithAll (killAll, sinkAll)
import XMonad.Hooks.DebugStack (debugStack, debugStackFull)
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.InsertPosition (Focus (Newer), Position (End, Master), insertPosition)
import XMonad.Hooks.ManageDocks (docks, manageDocks)
import XMonad.Hooks.ManageHelpers (composeOne, doCenterFloat, doFullFloat, doRectFloat, doSink, isDialog, isFullscreen, isInProperty, transience, (-?>), (~?))
import XMonad.Hooks.RefocusLast (isFloat, refocusLastLayoutHook, refocusLastWhen)
import XMonad.Hooks.ServerMode (serverModeEventHookCmd')
import XMonad.Hooks.ShowWName (SWNConfig (..), showWNameLogHook)
import XMonad.Hooks.StatusBar (StatusBarConfig, dynamicSBs, killAllStatusBars, startAllStatusBars, statusBarPropTo)
import XMonad.Hooks.StatusBar.PP (PP (..), shorten, wrap, xmobarAction, xmobarColor)
import XMonad.Hooks.WorkspaceHistory (workspaceHistoryHookExclude)
import XMonad.Layout.Decoration (DefaultShrinker, ModifiedLayout, Theme (..), shrinkText)
import XMonad.Layout.DecorationEx (DecorationEx, GenericTheme (..), GenericWidget (TitleWidget), SimpleStyle, StandardWidget, TextDecoration (TextDecoration), decorationEx, themeEx, titleW)
import XMonad.Layout.DraggingVisualizer (draggingVisualizer)
import XMonad.Layout.FocusTracking (focusTracking)
import qualified XMonad.Layout.MultiToggle as MT
import XMonad.Layout.MultiToggle.Instances (StdTransformers (MIRROR))
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Notebook (IncColumnN (IncColumnN), MirrorResize (MirrorExpand, MirrorShrink), Notebook (Notebook), SResize (SExpand, SShrink), ToggleMiddle (ToggleMiddle), ToggleSide (ToggleSide), ToggleStackDir (ToggleStackDir))
import XMonad.Layout.PaperPersistent (IncWindowIndex (IncWindowIndex), PaperPersistent (PaperPersistent))
import XMonad.Layout.PerScreen (ifWider)
import XMonad.Layout.Renamed (Rename (KeepWordsRight), renamed)
import XMonad.Layout.Simplest (Simplest (Simplest))
import XMonad.Layout.Spacing (Border (Border), Spacing, spacingRaw)
import XMonad.Layout.TabbedGeometryLocal (HorizontalTabPlacement (Top), HorizontalTabWidth (AutoBarWidth), HorizontalTabsAlignment (AlignTabsRight), SingleTabMode (ShowTab), TabbedGeometry (HorizontalTabs))
import XMonad.Layout.TwoPanePersistentLocal (TwoPanePersistent (TwoPanePersistent))
import XMonad.Layout.WindowSwitcherDecoration (windowSwitcherDecoration)
import XMonad.Prompt (ComplCaseSensitivity (CaseInSensitive), XPConfig (..), XPPosition (CenteredAt))
import XMonad.Prompt.FuzzyMatch (fuzzyMatch)
import XMonad.Prompt.Input (inputPrompt, (?+))
import XMonad.Prompt.XMonad (xmonadPromptC, xmonadPromptCT)
import qualified XMonad.StackSet as W
import XMonad.Util.ClickableWorkspaces (clickablePP)
import XMonad.Util.Cursor (setDefaultCursor)
import XMonad.Util.EZConfig (additionalKeysP, removeKeysP)
import XMonad.Util.Hacks (windowedFullscreenFixEventHook)
import XMonad.Util.NamedScratchpadLocal (NamedScratchpad (NS), defaultFloating, hideAllNamedScratchPads, namedScratchpadAction, namedScratchpadManageHook, nonFloating)
import XMonad.Util.Paste as P (sendKey)
import XMonad.Util.SpawnOnce (spawnOnOnce, spawnOnce)
import XMonad.Util.WorkspaceCompare (getWsCompare)

----------------------------------------------------------------------------------------------------
-- Main                                                                                           --
----------------------------------------------------------------------------------------------------
main :: IO ()
main = do
  numberOfScreens <- getScreens
  xmonad $
    dynamicProjects projects $
      withNavigation2DConfig myNav2DConf $
        dynamicSBs barSpawner $
          ewmh $
            addProfilesWithHistory myProfileConfig $
              docks (myConfig numberOfScreens)

myConfig n =
  def
    { borderWidth = myBorder,
      clickJustFocuses = True,
      focusFollowsMouse = True,
      normalBorderColor = background,
      focusedBorderColor = active,
      manageHook = myManageHook,
      handleEventHook = myHandleEventHook,
      layoutHook = myLayoutHook,
      logHook = myLogHook,
      modMask = myModMask,
      mouseBindings = myMouseBindings,
      startupHook = myStartupHook,
      terminal = myTerminal,
      workspaces = myWorkspaces
    }
    `additionalKeysP` myKeys n
    `removeKeysP` ["M-" ++ m ++ [n] | n <- ['1' .. '9'], m <- ["S-", ""]]

-- | Get number of screens
getScreens :: IO Int
getScreens = do
  screens <- do
    dpy <- X11.openDisplay ""
    rects <- X11.getScreenInfo dpy
    X11.closeDisplay dpy
    return rects
  pure $ length screens

----------------------------------------------------------------------------------------------------
-- Workspaces                                                                                     --
----------------------------------------------------------------------------------------------------

projects :: [Project]
projects =
  [ Project {pName = "Tmp", pDir = "/tmp", pApp1 = kitty, pApp1F = kittyF, pApp4 = br persB, pApp4F = brF persB, pStart = Just $ return ()},
    Project {pName = "Tmp2", pDir = "/tmp", pApp1 = kitty, pApp1F = kittyF, pApp4 = br persB, pApp4F = brF persB, pStart = Just $ return ()},
    Project {pName = "M", pDir = "~/.config", pApp1 = kitty, pApp1F = kittyF, pApp4 = br persB, pApp4F = brF persB, pStart = tBSpawn "M" persB},
    Project {pName = "Blank1", pDir = "~/Documents", pApp1 = kitty, pApp1F = kittyF, pApp4 = br persB, pApp4F = brF persB, pStart = Just $ return ()},
    Project {pName = "Work1", pDir = "~/Projects/DECSAM", pApp1 = kitty, pApp1F = kittyF, pApp4 = br workB, pApp4F = brF workB, pStart = Just $ return ()},
    Project {pName = "Work2", pDir = "~/Projects/DECSAM", pApp1 = kitty, pApp1F = kittyF, pApp4 = br workB, pApp4F = brF workB, pStart = Just $ return ()},
    Project {pName = "Films", pDir = "~/Videos/films", pApp1 = kitty, pApp1F = kittyF, pApp2 = deluge, pApp2F = delugeF, pApp4 = br filmB, pApp4F = brF filmB, pStart = fSpawn "Films"},
    Project {pName = "Scin-Main", pDir = "~/Projects/Scintilla/Main", pApp1 = kitty, pApp1F = kittyF, pApp2 = scinCont, pApp2F = scinContF, pApp3 = zathura, pApp3F = zathuraF, pApp4 = br univB, pApp4F = brF univB, pStart = tBSpawn "Scin-Main" univB},
    Project {pName = "Scin-Test", pDir = "~/Projects/Scintilla/Main", pApp1 = kitty, pApp1F = kittyF, pApp2 = scinCont, pApp2F = scinContF, pApp3 = zathura, pApp3F = zathuraF, pApp4 = br univB, pApp4F = brF univB, pStart = Just $ return ()}
  ]
  where
    kitty = bindFirst [kt "action launch_os_window", l (upPointer $ Wgl.runOrRaiseNext "kitty" (className =? "kitty"))]
    kittyF = upPointer $ spawn "kitty"

    sameForce r c = (upPointer $ Wgl.runOrRaiseNext r (className =? c), upPointer $ spawn r)
    (zathura, zathuraF) = sameForce "zathura" "Zathura"
    (deluge, delugeF) = sameForce "deluge" "Deluge-gtk"
    scinStart = "cd /home/oleete/Projects/Scintilla/Main; .venv/bin/python main.pyw"
    (scinCont, scinContF) = (upPointer $ Wgl.runOrRaiseNext scinStart (title =? "Scintilla Control"), upPointer $ spawn scinStart)

    persB = "google-chrome-stable-personal"
    univB = "google-chrome-stable-uni"
    workB = "google-chrome-stable-work"
    filmB = "google-chrome-stable-Films"
    br na = bindFirst [crm (P.sendKey controlMask xK_t), l (upPointer $ brS na)]
    brS na = Wgl.raiseNextMaybeCustomFocus2 bringWindow (brF na) (className =? na)
    brL na = Wgl.raiseNextMaybeCustomFocus3 (brF na) (className =? na)
    brF na = upPointer $ spawn (myBrowserClass ++ " --class=" ++ na ++ " --user-data-dir=/home/oleete/.config/browser/" ++ na)

    sl i = "sleep .1; " ++ i
    tBSpawn ws na = Just $ do spawnOn ws myTerminal; brL na
    bSpawn na = Just $ brL na
    oSpawn ws app = Just $ spawnOn ws $ sl app
    fSpawn ws = Just $ do spawnOn ws $ sl myBrowser; spawnOn ws ("sleep .2; " ++ myTerminal); spawnOn ws $ sl "deluge"

myWorkspaces :: [String]
myWorkspaces = map pName projects

myProfileConfig :: ProfileConfig
myProfileConfig =
  def
    { profiles = myProfiles,
      startingProfile = "Blank",
      workspaceExcludes = ["NSP"]
    }

myProfiles :: [Profile]
myProfiles =
  [ Profile
      { profileId = "Blank",
        profileWS =
          [ "Tmp",
            "Tmp2",
            "M",
            "Blank1",
            "Work1",
            "Work2",
            "Films"
          ]
      }
  ]

----------------------------------------------------------------------------------------------------
-- Applications                                                                                   --
----------------------------------------------------------------------------------------------------
myTerminal, myBrowser, myBrowserClass, kittyRemote, myTasks :: String
myTerminal = "kitty"
myBrowser = "/home/oleete/.config/bin/browser"
myBrowserClass = "google-chrome-stable"
kittyRemote = "kitty @ --to unix:/tmp/mykitty-$(xdotool getactivewindow getwindowpid) "
myTasks = myBrowserClass ++ " --user-data-dir=/home/oleete/.config/browser/ticktick  --class=ticktick  --app=https://ticktick.com/webapp"

scratchpads :: [NamedScratchpad]
scratchpads =
  [ NS "discord" (myBrowserClass ++ " --user-data-dir=/home/oleete/.config/browser/discord  --class=discord  --app=https://discord.com/channels/@me") (className =? "discord") defaultFloating,
    NS "youtubeMusic" (myBrowserClass ++ " --user-data-dir=/home/oleete/.config/browser/youtubeMusic  --class=youtubemusic  --app=https://music.youtube.com/") (className =? "youtubemusic") defaultFloating,
    NS "ruler" "kruler" (resource =? "kruler") nonFloating,
    NS "sysMon" "kitty --class=sysMon btop" (resource =? "sysMon") nonFloating
  ]

----------------------------------------------------------------------------------------------------
-- Theme                                                                                          --
----------------------------------------------------------------------------------------------------
background, foreground, dull, active, yellow, green :: String
background = "#222436"
foreground = "#c8d3f5"
dull = "#636da6"
active = "#82AAFF"
yellow = "#ffc777"
green = "#c3e88d"

-- sizes
gap :: Integer
gap = 3

reSize :: Rational
reSize = 1 / 12

moreReSize :: Rational
moreReSize = 1 / 4

myBorder :: Dimension
myBorder = 3

myWideFont :: String
myWideFont = "xft:Ubuntu Nerd Font:" ++ "style=Regular:pixelsize=180:hinting=true"

myShowWNameTheme :: SWNConfig
myShowWNameTheme =
  def
    { swn_font = myWideFont,
      swn_fade = 0.3,
      swn_bgcolor = active,
      swn_color = background
    }

myDecoTheme :: Theme
myDecoTheme =
  def
    { inactiveColor = background,
      inactiveBorderColor = background,
      inactiveTextColor = background,
      activeColor = active,
      activeBorderColor = active,
      activeTextColor = active,
      decoHeight = 6
    }

myTabTheme :: Theme
myTabTheme =
  def
    { inactiveColor = background,
      inactiveBorderColor = background,
      inactiveTextColor = foreground,
      activeColor = active,
      activeBorderColor = active,
      activeTextColor = background,
      decoHeight = 18,
      fontName = "xft:Ubuntu Nerd Font:weight=bold:pixelsize=12:antialias=true:hinting=true"
    }

myPromptConfig :: XPConfig
myPromptConfig =
  def
    { font = "xft:Ubuntu Nerd Font:weight=bold:pixelsize=16:antialias=true:hinting=true",
      bgColor = background,
      fgColor = foreground,
      bgHLight = background,
      fgHLight = active,
      borderColor = active,
      promptBorderWidth = 3,
      alwaysHighlight = True,
      maxComplColumns = Just 1,
      maxComplRows = Just 15,
      height = 35,
      position = CenteredAt (1 / 3) (1 / 8),
      searchPredicate = fuzzyMatch,
      autoComplete = Nothing,
      completionKey = (noModMask, xK_Down),
      prevCompletionKey = (noModMask, xK_Up),
      complCaseSensitivity = CaseInSensitive
    }

----------------------------------------------------------------------------------------------------
-- Layouts                                                                                        --
----------------------------------------------------------------------------------------------------

mySpacing :: l a -> ModifiedLayout Spacing l a
mySpacing = spacingRaw False (Border gap gap gap gap) True (Border gap gap gap gap) True

data FULLNB = FULLNB deriving (Read, Show, Eq, Typeable)

instance MT.Transformer FULLNB Window where
  transform FULLNB x k = k (smartBorders Full) (const x)

data FULLTAB = FULLBAR deriving (Read, Show, Eq, Typeable)

instance MT.Transformer FULLTAB Window where
  transform FULLBAR x k = k (fullTab $ mySpacing Simplest) (const x)
    where
      fullTab :: l Window -> ModifiedLayout (DecorationEx TextDecoration StandardWidget TabbedGeometry DefaultShrinker) l Window
      fullTab =
        decorationEx
          shrinkText
          ((themeEx myTabTheme) {exWidgetsCenter = [TitleWidget]})
          TextDecoration
          (HorizontalTabs ShowTab Top AlignTabsRight AutoBarWidth 30)

data TWOPANE = TWOPANE deriving (Read, Show, Eq, Typeable)

instance MT.Transformer TWOPANE Window where
  transform TWOPANE x k = k (mySpacing $ TwoPanePersistent Nothing reSize (1 / 2)) (const x)

data PAPER = PAPER deriving (Read, Show, Eq, Typeable)

instance MT.Transformer PAPER Window where
  transform PAPER x k = k (mySpacing $ PaperPersistent (-1) (1 / 2) (1 / 20)) (const x)

myLayoutHook =
  renamed [KeepWordsRight 1] $
    refocusLastLayoutHook $
      focusTracking $
        MT.mkToggle (MT.single MIRROR) $
          MT.mkToggle (MT.single FULLNB) $
            MT.mkToggle (MT.single FULLBAR) $
              myDeco $
                draggingVisualizer $
                  MT.mkToggle (MT.single TWOPANE) $
                    MT.mkToggle (MT.single PAPER) $
                      mySpacing
                        notebookLayout
  where
    myDeco = windowSwitcherDecoration shrinkText myDecoTheme
    notebookBigScreen = Notebook True False True 1 2 moreReSize reSize 3 (2 / 3) 1
    notebookLaptop = Notebook False False False 1 2 moreReSize reSize 2 (2 / 3) 1
    notebookLayout = ifWider 1920 notebookBigScreen notebookLaptop

----------------------------------------------------------------------------------------------------
-- Keybindings                                                                                    --
----------------------------------------------------------------------------------------------------
myNav2DConf :: Navigation2DConfig
myNav2DConf = def

myModMask :: KeyMask
myModMask = mod4Mask

{-
 ┏━━━━━━━┳━━━━━━━┳━━━━━━━┳━━━━━━━┳━━━━━━━┓                                   ┏━━━━━━━┳━━━━━━━┳━━━━━━━┳━━━━━━━┳━━━━━━━┓
 ┃   -   ┃fullWin┃proFind┃cmdPale┃   -   ┃                                   ┃winDown┃winRght┃ detach┃ float ┃   -   ┃
 ┣━━━━━━━╋━━━━━━━╋━━━━━━━╋━━━━━━━╋━━━━━━━┫                                   ┣━━━━━━━╋━━━━━━━╋━━━━━━━╋━━━━━━━╋━━━━━━━┫
 ┃  pws1 ┃  pws2 ┃  pws3 ┃  pws4 ┃  pws0 ┃                                   ┃winLeft┃  app1 ┃  app2 ┃  app3 ┃  app4 ┃
 ┣━━━━━━━╋━━━━━━━╋━━━━━━━╋━━━━━━━╋━━━━━━━╋━━━━━━━┳━━━━━━━┓   ┏━━━━━━━┳━━━━━━━╋━━━━━━━╋━━━━━━━╋━━━━━━━╋━━━━━━━╋━━━━━━━┫
 ┃fullScr┃fullBar┃fullCen┃twoPane┃ paper ┃nspAway┃nextScr┃   ┃   -   ┃  kill ┃ winUp ┃ Master┃ decCol┃ incCol┃   -   ┃
 ┗━━━━━━━╋━━━━━━━╋━━━━━━━╋━━━━━━━╋━━━━━━━╋━━━━━━━╋━━━━━━━┫   ┣━━━━━━━╋━━━━━━━╋━━━━━━━╋━━━━━━━╋━━━━━━━╋━━━━━━━╋━━━━━━━┛
         ┃   -   ┃   -   ┃       ┃tabPrev┃ wsLast┃winPrev┃   ┃winNext┃  term ┃tabNext┃       ┃   -   ┃   -   ┃
         ┗━━━━━━━┻━━━━━━━┛       ┗━━━━━━━┻━━━━━━━┻━━━━━━━┛   ┗━━━━━━━┻━━━━━━━┻━━━━━━━┛       ┗━━━━━━━┻━━━━━━━┛
-}
myKeys :: Int -> [(String, X ())]
myKeys n =
  [ ("<XF86MonBrightnessDown>", spawn "/home/oleete/.config/bin/brightness -dec 5"),
    ("<XF86MonBrightnessUp>", spawn "/home/oleete/.config/bin/brightness -inc 5"),
    ("<XF86AudioLowerVolume>", spawn "/home/oleete/.config/bin/volume set-sink-volume @DEFAULT_SINK@ -5%"),
    ("<XF86AudioRaiseVolume>", spawn "/home/oleete/.config/bin/volume set-sink-volume @DEFAULT_SINK@ +5%"),
    ("<XF86AudioMute>", spawn "/home/oleete/.config/bin/volume set-sink-volume @DEFAULT_SINK@ 0%"),
    ("<XF86Display>", spawn "/home/oleete/.config/bin/displayctl"),
    ("<XF86AudioPlay>", spawn "playerctl play-pause"),
    ("<XF86AudioStop>", spawn "playerctl stop"),
    ("<XF86AudioPause>", spawn "playerctl pause"),
    ("<XF86AudioPrev>", spawn "playerctl previous"),
    ("<XF86AudioNext>", spawn "playerctl next"),
    ("<Print>", spawn "/home/oleete/.config/bin/screencapt area"),
    ("M-p", myFuncPrompt myPromptConfig),
    ("M-f", mySwitchProfilePrompt myPromptConfig),
    ("M-<Esc>", upPointer $ sequence_ $ hideAllNamedScratchPads scratchpads),
    ("M-<Return>", upPointer $ spawn myTerminal),
    ("M-S-<Return>", upPointer $ spawn myTerminal),
    ("M-<Backspace>", bindFirst [nv "DeleteBuffer", kt "action close_window_c", crm (P.sendKey controlMask xK_w), l kill]),
    ("M-S-<Backspace>", bindFirst [kt "action close_window_c", l kill]),
    ("M-u", bindFirst [kt "action detach_window", crm (P.sendKey shiftMask xK_w), l (return ())]),
    ("M-n", runProjectApp1),
    ("M-e", runProjectApp2),
    ("M-i", runProjectApp3),
    ("M-o", runProjectApp4),
    ("M-S-n", runProjectApp1Force),
    ("M-S-e", runProjectApp2Force),
    ("M-S-i", runProjectApp3Force),
    ("M-S-o", runProjectApp4Force),
    ("M-/", upPointer $ Wgl.raiseNextMaybeCustomFocus2 bringWindow (spawn myTasks) (className =? "ticktick")),
    ("M-<Left>", bindFirst [nv "tabprev", kt "action previous_tab", l (P.sendKey (controlMask .|. shiftMask) xK_Tab)]),
    ("M-<Right>", bindFirst [nv "tabnext", kt "action next_tab", l (P.sendKey controlMask xK_Tab)]),
    ("M-<Down>", upPointer $ windows W.focusDown),
    ("M-<Up>", upPointer $ windows W.focusUp),
    ("M-w", bindFirst [kt "action toggle_stack", crm (spawn "/home/oleete/.config/bin/chromeFull"), l (P.sendKey noModMask xK_F11)]),
    ("M-z", toggleLayout FULLNB),
    ("M-x", toggleLayout FULLBAR),
    ("M-c", toggleLayout TWOPANE),
    ("M-v", toggleLayout PAPER),
    ("M-h", bindFirst [nMoveLeft, kMoveLeft, moveLeft]),
    ("M-j", bindFirst [nMoveBottom, kMoveBottom, moveBottom]),
    ("M-k", bindFirst [nMoveTop, kMoveTop, moveTop]),
    ("M-l", bindFirst [nMoveRight, kMoveRight, moveRight]),
    ("M-S-h", upPointer $ bindByLayout [("PaperPersistent", windows W.swapUp), ("", windowSwap L False)]),
    ("M-S-j", upPointer $ windowSwap D False),
    ("M-S-k", upPointer $ windowSwap U False),
    ("M-S-l", upPointer $ bindByLayout [("PaperPersistent", windows W.swapDown), ("", windowSwap R False)]),
    ("M-m", upPointer $ swapPromote' False),
    ("M-y", upPointer $ withFocused toggleFloat),
    ("M-S-y", upFocus sinkAll),
    ("M-,", sendMessage (IncMasterN (-1))),
    ("M-.", sendMessage (IncMasterN 1)),
    ("M-S-,", sendMessage (IncColumnN (-1))),
    ("M-S-.", sendMessage (IncColumnN 1)),
    ("M-[", sendMessage Shrink),
    ("M-]", sendMessage Expand),
    ("M-S-[", sendMessage MirrorShrink),
    ("M-S-]", sendMessage MirrorExpand),
    ("M--", sendMessage SShrink),
    ("M-=", sendMessage SExpand),
    ("M-d", withNthProfileWorkspace 2 W.greedyView),
    ("M-a", withNthProfileWorkspace 3 W.greedyView),
    ("M-r", withNthProfileWorkspace 4 W.greedyView),
    ("M-s", withNthProfileWorkspace 5 W.greedyView),
    ("M-t", withNthProfileWorkspace 6 W.greedyView),
    ("M-S-d", withNthProfileWorkspace 2 W.shift),
    ("M-S-a", withNthProfileWorkspace 3 W.shift),
    ("M-S-r", withNthProfileWorkspace 4 W.shift),
    ("M-S-s", withNthProfileWorkspace 5 W.shift),
    ("M-S-t", withNthProfileWorkspace 6 W.shift),
    ("M-<Tab>", tabCommand),
    ("M-S-<Tab>", shiftTabCommand),
    ("M-<Space>", upFocus $ toggleWS' ["NSP"]),
    ("M-S-<Space>", upFocus $ shiftToggleWS' ["NSP"])
  ]
  where
    toggleFloat w =
      windows
        ( \s ->
            if M.member w (W.floating s)
              then W.sink w s
              else W.float w (W.RationalRect (1 / 4) (1 / 4) (1 / 2) (1 / 2)) s
        )
    tabCommand =
      if n > 1
        then upPointer nextScreen
        else upFocus $ toggleWS' ["NSP"]

    shiftTabCommand =
      if n > 1
        then upPointer shiftNextScreen
        else upFocus $ shiftToggleWS' ["NSP"]

    mySwitchProfilePrompt c = do
      ps <- profileIds
      xmonadPromptCT
        "Profile"
        ( map (\p -> (p, switchToProfile p)) ps
            ++ [ ("Add", addWSToProfilePrompt c),
                 ("Remove", removeWSFromProfilePrompt c),
                 ("Switch", switchProfileWSPrompt c),
                 ("Send", shiftProfileWSPrompt c),
                 ("Clear", killAll)
               ]
        )
        c

myFuncPrompt :: XPConfig -> X ()
myFuncPrompt c =
  xmonadPromptC
    [ ("Lock", spawn "slock"),
      ("Play", spawn "playerctl play"),
      ("Pause", spawn "playerctl pause"),
      ("DND", spawn "/home/oleete/.config/bin/toggle DND_menu"),
      ("Discord", upPointer $ namedScratchpadAction scratchpads "discord"),
      ("Apps", spawn "rofi -matching fuzzy -show drun -show-icons"),
      ("Skip", spawn "playerctl next"),
      ("Previous", spawn "playerctl previous"),
      ("Music", upPointer $ namedScratchpadAction scratchpads "youtubeMusic"),
      ("Sound", spawn "/home/oleete/.config/bin/soundctl sink-menu"),
      ("SoundInput", spawn "/home/oleete/.config/bin/soundctl source-menu"),
      ("Display", spawn "/home/oleete/.config/bin/displayctl"),
      ("Network", spawn "networkmanager_dmenu"),
      ("Bluetooth", spawn "/home/oleete/.config/bin/rofi-bluetooth"),
      ("Volume", inputPrompt c "Volume" ?+ (\v -> spawn ("/home/oleete/.config/bin/volume set-sink-volume @DEFAULT_SINK@ " ++ v ++ "%"))),
      ("Brightness", inputPrompt c "Brightness" ?+ (spawn . (++) "/home/oleete/.config/bin/brightness -set ")),
      ("SysMon", upPointer $ namedScratchpadAction scratchpads "sysMon"),
      ("Ruler", upPointer $ namedScratchpadAction scratchpads "ruler"),
      ("ColorPicker", spawn "/home/oleete/.config/bin/colorPicker"),
      ("Screenshot", spawn "/home/oleete/.config/bin/screencapt"),
      ("ScreenshotArea", spawn "/home/oleete/.config/bin/screencapt area"),
      ("Screencap", spawn "/home/oleete/.config/bin/screencast"),
      ("ScreencapArea", spawn "/home/oleete/.config/bin/screencast area"),
      ("ScreencapGif", spawn "/home/oleete/.config/bin/screencast gif-last"),
      ("Screenkey", spawn "killall screenkey || screenkey"),
      ("ScreenkeySettings", spawn "screenkey --show-settings"),
      ("FullScreen", toggleLayout FULLNB),
      ("FullBar", toggleLayout FULLBAR),
      ("TwoPane", toggleLayout TWOPANE),
      ("PaperLayout", toggleLayout PAPER),
      ("Rotate", toggleLayout MIRROR),
      ("Mirror", upFocus $ sendMessage ToggleSide),
      ("MirrorStack", upFocus $ sendMessage ToggleStackDir),
      ("CenterMain", upFocus $ sendMessage ToggleMiddle),
      ("ShrinkStack", sendMessage SShrink),
      ("ExpandStack", sendMessage SExpand),
      ("Status", spawn "/home/oleete/.config/bin/statusNotify"),
      ("Logout", spawn "pkill xmonad"),
      ("Suspend", spawn "systemctl suspend"),
      ("Hibernate", spawn "systemctl hibernate"),
      ("shutdown", spawn "systemctl poweroff"),
      ("Reboot", spawn "systemctl reboot")
    ]
    c

myMouseBindings :: XConfig l -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings XConfig {} =
  M.fromList
    [ ( (myModMask, button1),
        \w ->
          focus w
            >> mouseMoveWindow w
            >> windows W.shiftMaster
      ),
      ( (myModMask, button3),
        \w ->
          focus w
            >> mouseResizeWindow w
            >> windows W.shiftMaster
      )
    ]

----------------------------------------------------------------------------------------------------
-- Startup                                                                                        --
----------------------------------------------------------------------------------------------------

myStartupHook :: X ()
myStartupHook = do
  killAllStatusBars
  setDefaultCursor xC_left_ptr
  spawn "variety -n"
  spawnOnce "picom -b --config ~/.config/picom/picom.conf"
  spawnOnce "insync start; insync hide"
  spawnOnce "/home/oleete/.config/bin/startupScript"
  spawnOnce "/home/oleete/.config/bin/connect_screen.py"
  spawnOnce "kitty --class=sysMon btop"
  spawnOnce "sleep .5; /home/oleete/.cabal/bin/xmonadctl-exe close-scratchpads"

----------------------------------------------------------------------------------------------------
-- Bar/PP                                                                                         --
----------------------------------------------------------------------------------------------------

barSpawner :: ScreenId -> X StatusBarConfig
barSpawner (S sid) =
  pure $
    statusBarPropTo ("_XMONAD_LOG_" ++ show sid) ("/home/oleete/.config/xmobar/xmobarLaunch " ++ show sid) myPP

myPP :: X PP
myPP =
  clickablePP
    <=< excludeWSPP
    $ def
      { ppCurrent = underlineMod active,
        ppVisible = xmobarColor active "",
        ppHidden = xmobarColor dull "",
        ppTitle = underlineMod yellow . shorten 30,
        ppLayout = const "",
        ppSep = xmobarColor foreground "" " | ",
        ppExtras = [profileLogger (clickableProf $ underlineMod green) (clickableProf $ xmobarColor dull "")],
        ppOrder = \(ws : _ : t : p) -> t : ws : p,
        ppSort = do
          cmp <- getWsCompare
          return $ sortBy (flip cmp `on` W.tag)
      }
  where
    underlineMod c = xmobarColor c "" . wrap ("<box type=Bottom width=3 mt=2 color=" ++ c ++ ">") "</box>"
    clickableProf :: (String -> String) -> String -> String
    clickableProf f p = xmobarAction ("/home/oleete/.cabal/bin/xmonadctl-exe profile-" ++ p) "1" $ f p

----------------------------------------------------------------------------------------------------
-- Log                                                                                            --
----------------------------------------------------------------------------------------------------

myLogHook :: X ()
myLogHook = do
  masterHistoryHook
  workspaceHistoryHookExclude ["NSP"]
  showWNameLogHook myShowWNameTheme

----------------------------------------------------------------------------------------------------
-- New Window Actions                                                                             --
----------------------------------------------------------------------------------------------------

myManageHook :: ManageHook
myManageHook =
  manageSpecific
    <+> manageDocks
    <+> insertPosition Master Newer
    <+> namedScratchpadManageHook scratchpads
    <+> manageSpawn
  where
    manageSpecific =
      composeOne
        [ resource =? "desktop_window" -?> doIgnore,
          resource =? "prusa-slicer" -?> doSink <+> insertPosition End Newer,
          title =? "Scintilla Control" -?> scinTestShift $ doSink <+> insertPosition End Newer,
          title =? "Scintilla Option Editor" -?> scinTestShift doCenterFloat,
          title =? "Scintilla Build Editor" -?> scinTestShift doCenterFloat,
          title =? "Scintilla Connection Manager" -?> scinTestShift doCenterFloat,
          title =? "Scintilla Position Override" -?> scinTestShift doCenterFloat,
          title =? "Scintilla File List" -?> scinTestShift doCenterFloat,
          resource =? "pavucontrol" -?> doRectFloat (W.RationalRect (8 / 1920) (31 / 1080) (600 / 1920) (800 / 1080)),
          className =? "Nm-connection-editor" -?> doRectFloat (W.RationalRect (8 / 1920) (31 / 1080) (600 / 1920) (800 / 1080)),
          className =? "Nm-applet" -?> doRectFloat (W.RationalRect (8 / 1920) (31 / 1080) (600 / 1920) (800 / 1080)),
          className =? "Tlp-UI" -?> doRectFloat (W.RationalRect (8 / 1920) (31 / 1080) (600 / 1920) (800 / 1080)),
          className =? "Blueberry.py" -?> doRectFloat (W.RationalRect (8 / 1920) (31 / 1080) (600 / 1920) (800 / 1080)),
          className =? "GCal" -?> doRectFloat bigFloat,
          className =? "WrkGCal" -?> doRectFloat bigFloat,
          resource =? "sysMon" -?> doRectFloat (W.RationalRect (1 / 8) (1 / 8) (3 / 4) (3 / 4)),
          resource =? "console" -?> doRectFloat (W.RationalRect (4 / 7) (4 / 7) (2 / 5) (2 / 5)),
          className =? "youtubemusic" -?> doRectFloat halfNhalf,
          className =? "discord" -?> doRectFloat halfNhalf,
          className =? "ticktick" -?> insertPosition End Newer,
          resource =? "kruler" -?> doFloat,
          transience,
          isBrowserDialog -?> doCenterFloat,
          isRole =? "GtkFileChooserDialog" -?> doCenterFloat,
          isRole =? "pop-up" -?> doCenterFloat,
          isInProperty
            "_NET_WM_WINDOW_TYPE"
            "_NET_WM_WINDOW_TYPE_SPLASH"
            -?> doCenterFloat,
          isFullscreen -?> doFullFloat,
          fmap not isDialog -?> insertPosition End Newer
        ]
    isBrowserDialog = isDialog <&&> className =? myBrowserClass
    halfNhalf = W.RationalRect (1 / 4) (1 / 4) (1 / 2) (1 / 2)
    bigFloat = W.RationalRect (1 / 8) (1 / 8) (3 / 4) (3 / 4)
    scinTestShift pos = doShift "Scin-Test" <+> pos

----------------------------------------------------------------------------------------------------
-- HangleEventHook                                                                                --
----------------------------------------------------------------------------------------------------

myHandleEventHook :: Event -> X All
myHandleEventHook =
  handleEventHook def
    <+> XMonad.Util.Hacks.windowedFullscreenFixEventHook
    <+> myServerModeEventHook
    <+> refocusLastWhen isFloat

----------------------------------------------------------------------------------------------------
-- Helper Functions                                                                               --
----------------------------------------------------------------------------------------------------

isRole :: Query String
isRole = stringProperty "WM_WINDOW_ROLE"

-- binding shortcuts
upFocus :: X () -> X ()
upFocus a = sequence_ [a, focusUnderPointer]

upPointer :: X () -> X ()
upPointer a = sequence_ [a, updatePointer (0.5, 0.5) (0.25, 0.25)]

toggleLayout :: (MT.Transformer t a, Typeable a) => t -> X ()
toggleLayout layout = sequence_ [withFocused $ windows . W.sink, sendMessage $ MT.Toggle layout, updatePointer (0.5, 0.5) (0.25, 0.25)]

-- | Bind key on chrome or any leftover app
l, crm :: X () -> (Query Bool, X ())
l raw = (pure True, raw)
crm raw = (isRole =? "browser", raw)

-- | Bind key on neovim or kitty
nv, kt :: String -> (Query Bool, X ())
nv command = (title ~? "Neovim_", spawn ("/home/oleete/.config/bin/nvrWS " ++ command))
kt remote = (className =? "kitty", spawn (kittyRemote ++ remote))

-- | Bring the window
bringWindow :: (Eq s, Eq i, Ord a) => a -> W.StackSet i l a s sd -> W.StackSet i l a s sd
bringWindow w ws = W.focusWindow w $ W.shiftWinDown (W.currentTag ws) w ws

-- | Movement
nMoveLeft, nMoveBottom, nMoveTop, nMoveRight :: (Query Bool, X ())
nMoveLeft = nv "NavigateLeft"
nMoveBottom = nv "NavigateBottom"
nMoveTop = nv "NavigateTop"
nMoveRight = nv "NavigateRight"

kMoveLeft, kMoveBottom, kMoveTop, kMoveRight :: (Query Bool, X ())
kMoveLeft = kt "focus-window --match neighbor:left || /home/oleete/.cabal/bin/xmonadctl-exe winGo-H"
kMoveBottom = kt "focus-window --match neighbor:bottom || /home/oleete/.cabal/bin/xmonadctl-exe winGo-J"
kMoveTop = kt "focus-window --match neighbor:top || /home/oleete/.cabal/bin/xmonadctl-exe winGo-K"
kMoveRight = kt "focus-window --match neighbor:right || /home/oleete/.cabal/bin/xmonadctl-exe winGo-L"

moveLeft, moveBottom, moveTop, moveRight :: (Query Bool, X ())
moveLeft = l $ bindByLayout [("PaperPersistent", sendMessage (IncWindowIndex (-1))), ("", upPointer (windowGo L False))]
moveBottom = l $ upPointer (windowGo D False)
moveTop = l $ upPointer (windowGo U False)
moveRight = l $ bindByLayout [("PaperPersistent", sendMessage (IncWindowIndex 1)), ("", upPointer (windowGo R False))]

----------------------------------------------------------------------------------------------------
-- Server Commands                                                                                --
----------------------------------------------------------------------------------------------------

myServerModeEventHook :: Event -> X All
myServerModeEventHook = serverModeEventHookCmd' $ return $ myCommands ++ sendTo
  where
    sendTo = map (\p -> ("profile-" ++ p.profileId, switchToProfile p.profileId)) myProfiles
    myCommands =
      [ ("winGo-h", bindFirst [kMoveLeft, moveLeft]),
        ("winGo-j", bindFirst [kMoveBottom, moveBottom]),
        ("winGo-k", bindFirst [kMoveTop, moveTop]),
        ("winGo-l", bindFirst [kMoveRight, moveRight]),
        ("winGo-H", bindFirst [moveLeft]),
        ("winGo-J", bindFirst [moveBottom]),
        ("winGo-K", bindFirst [moveTop]),
        ("winGo-L", bindFirst [moveRight]),
        ("close-scratchpads", sequence_ $ hideAllNamedScratchPads scratchpads),
        ("project-browser", runProjectApp4),
        ("sendF", P.sendKey noModMask xK_f),
        ("sendF11", P.sendKey noModMask xK_F11),
        ("dump-stack", debugStack),
        ("dump-full-stack", debugStackFull),
        ( "restart-bars",
          do
            killAllStatusBars
            startAllStatusBars
            spawn "variety -n"
        )
      ]
