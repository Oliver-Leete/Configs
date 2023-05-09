{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
import qualified Data.Map                               as M
import           Data.Monoid
import           Graphics.X11.Types
import qualified Graphics.X11.Xinerama                  as X11
import qualified Graphics.X11.Xlib                      as X11
import           XMonad                                 hiding ((|||))
import qualified XMonad.StackSet                        as W

import           XMonad.Actions.CopyWindow
import           XMonad.Actions.CycleWS                 (nextScreen,
                                                         shiftNextScreen)
import           XMonad.Actions.CycleWSLocal
import           XMonad.Actions.DynamicProjectsLocal
import           XMonad.Actions.DynamicWorkspaces
import           XMonad.Actions.Navigation2D
import           XMonad.Actions.PerWindowKeys
import           XMonad.Actions.SpawnOn
import           XMonad.Actions.SwapPromote
import           XMonad.Actions.UpdateFocus
import           XMonad.Actions.UpdatePointer
import           XMonad.Actions.WindowGoLocal
import           XMonad.Actions.WithAll                 (sinkAll)

import           XMonad.Hooks.DebugStack
import           XMonad.Hooks.EwmhDesktops              (ewmh)
import           XMonad.Hooks.InsertPosition
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.RefocusLast
import           XMonad.Hooks.ServerMode
import           XMonad.Hooks.ShowWName
import           XMonad.Hooks.StatusBar
import           XMonad.Hooks.StatusBar.PP
import           XMonad.Hooks.WorkspaceHistory          (workspaceHistoryHookExclude)

import           XMonad.Layout.DraggingVisualizer
import           XMonad.Layout.FourColumns
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Notebook
import           XMonad.Layout.PerScreen
import           XMonad.Layout.PerWorkspace
import           XMonad.Layout.SimpleDecoration
import           XMonad.Layout.SimpleFocus
import           XMonad.Layout.Spacing
import           XMonad.Layout.TwoPanePersistentLocal
import           XMonad.Layout.WindowSwitcherDecoration

import           XMonad.Layout.Decoration
import           XMonad.Util.ClickableWorkspaces
import           XMonad.Util.Cursor
import           XMonad.Util.EZConfig
import           XMonad.Util.Hacks
import           XMonad.Util.NamedScratchpadLocal
import           XMonad.Util.Paste                      as P
import           XMonad.Util.SpawnOnce

----------------------------------------------------------------------------------------------------
-- Main                                                                                           --
----------------------------------------------------------------------------------------------------
main :: IO ()
main = do
    numberOfScreens <- getScreens
    xmonad
        $ dynamicProjects (projects numberOfScreens)
        $ withNavigation2DConfig myNav2DConf
        $ dynamicSBs barSpawner
        $ ewmh
        $ docks (myConfig numberOfScreens)

myConfig n = def
        { borderWidth        = myBorder
        , clickJustFocuses   = True
        , focusFollowsMouse  = True
        , normalBorderColor  = background
        , focusedBorderColor = active
        , manageHook         = myManageHook n
        , handleEventHook    = myHandleEventHook n
        , layoutHook         = myLayoutHook
        , logHook            = myLogHook
        , modMask            = myModMask
        , mouseBindings      = myMouseBindings
        , startupHook        = myStartupHook
        , terminal           = myTerminal
        , workspaces         = myWorkspaces
        } `additionalKeysP` myKeys n

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

projects :: Int -> [Project]
projects n =
    [ Project { pName = "Tmp",        pDir = "/tmp",                          pApp1 = kitty,    pApp1F = kittyF,    pApp2 = return (), pApp2F = return (), pApp3 = return (), pApp3F = return (), pStart = Just $ return () }
    , Project { pName = "Tmp2",       pDir = "/tmp",                          pApp1 = kitty,    pApp1F = kittyF,    pApp2 = return (), pApp2F = return (), pApp3 = return (), pApp3F = return (), pStart = Just $ return () }

    , Project { pName = "Home",       pDir = "~/PersonalDrive",               pApp1 = kitty,    pApp1F = kittyF,    pApp2 = zathura,   pApp2F = zathuraF,  pApp3 = return (), pApp3F = return (), pStart = browSpawn "Home" }
    , Project { pName = "CodeTuts",   pDir = "~/Projects/rustBook",           pApp1 = kitty,     pApp1F = kittyF,     pApp2 = return (), pApp2F = return (), pApp3 = return (), pApp3F = return (), pStart = termBrowSpawn "CodeTuts" }
    , Project { pName = "Print",      pDir = "~/Projects/Printing",           pApp1 = prusa,    pApp1F = prusaF,    pApp2 = return (), pApp2F = return (), pApp3 = return (), pApp3F = return (), pStart = oneSpawn "Print" "flatpak run com.prusa3d.PrusaSlicer" }
    , Project { pName = "Games",      pDir = "~/Documents",                   pApp1 = steam,    pApp1F = steamF,    pApp2 = return (), pApp2F = return (), pApp3 = return (), pApp3F = return (), pStart = oneSpawn "Games" "steam" }
    , Project { pName = "Films",      pDir = "~/Videos/films",                pApp1 = kitty,    pApp1F = kittyF,    pApp2 = mpv,       pApp2F = mpvF,      pApp3 = deluge,    pApp3F = delugeF,   pStart = filmSpawn "Films" }
    , Project { pName = "Dnd",        pDir = "~/Projects/Rpgs",               pApp1 = kitty,    pApp1F = kittyF,    pApp2 = zathura,   pApp2F = zathuraF,  pApp3 = return (), pApp3F = return (), pStart = browSpawn "Dnd" }

    , Project { pName = "Configs",    pDir = "~/.config",                     pApp1 = kitty,     pApp1F = kittyF,     pApp2 = return (), pApp2F = return (), pApp3 = return (), pApp3F = return (), pStart = termBrowSpawn "Configs" }
    , Project { pName = "QMK",        pDir = "~/Projects/qmk_firmware",       pApp1 = kitty,     pApp1F = kittyF,     pApp2 = return (), pApp2F = return (), pApp3 = return (), pApp3F = return (), pStart = termBrowSpawn "QMK" }
    , Project { pName = "ZMK",        pDir = "~/Projects/zmk-config",         pApp1 = kitty,     pApp1F = kittyF,     pApp2 = return (), pApp2F = return (), pApp3 = return (), pApp3F = return (), pStart = termBrowSpawn "ZMK" }

    , Project { pName = "Wrk",        pDir = "~/UniDrive",                    pApp1 = kitty,    pApp1F = kittyF,    pApp2 = zathura,   pApp2F = zathuraF,  pApp3 = return (), pApp3F = return (), pStart = browSpawn "Wrk" }
    , Project { pName = "WrkNotes",   pDir = "~/Projects/Thesis/Notes",       pApp1 = obsidian, pApp1F = obsidianF, pApp2 = zathura,   pApp2F = zathuraF,  pApp3 = return (), pApp3F = return (), pStart = oneSpawn "WrkNotes" "flatpak run md.obsidian.Obsidian" }
    , Project { pName = "Thesis",     pDir = "~/Projects/Thesis/thesis",      pApp1 = kitty,     pApp1F = kittyF,     pApp2 = zathura,   pApp2F = zathuraF,  pApp3 = foxit,     pApp3F = foxitF,    pStart = termBrowSpawn "Thesis" }
    , Project { pName = "Sim",        pDir = "~/Projects/PowderModel",        pApp1 = kitty,     pApp1F = kittyF,     pApp2 = return (), pApp2F = return (), pApp3 = return (), pApp3F = return (), pStart = termBrowSpawn "Sim" }
    , Project { pName = "Exp",        pDir = "~/Projects/JuliaPlotting",      pApp1 = kitty,     pApp1F = kittyF,     pApp2 = return (), pApp2F = return (), pApp3 = return (), pApp3F = return (), pStart = termBrowSpawn "Exp" }
    , Project { pName = "Scripts",    pDir = "~/Projects/Thesis/scripts",     pApp1 = kitty,     pApp1F = kittyF,     pApp2 = zathura,   pApp2F = zathuraF,  pApp3 = hdfview,   pApp3F = hdfviewF,  pStart = termBrowSpawn "Scripts" }
    , Project { pName = "Comments",   pDir = "~/Projects/Thesis/thesis",      pApp1 = kitty,     pApp1F = kittyF,     pApp2 = zathura,   pApp2F = zathuraF,  pApp3 = foxit,     pApp3F = foxitF,    pStart = commentSpawn "Comments" }
    , Project { pName = "ANSYS",      pDir = "~/Projects/ANSYSpowderModel",   pApp1 = kitty,     pApp1F = kittyF,     pApp2 = paraview,  pApp2F = paraviewF, pApp3 = return (), pApp3F = return (), pStart = termBrowSpawn "ANSYS" }

    , Project { pName = "Scin-Main",  pDir = "~/Projects/Scintilla/Main",     pApp1 = kitty,     pApp1F = kittyF,     pApp2 = scinCont,  pApp2F = return (), pApp3 = return (), pApp3F = return (), pStart = termBrowSpawn "Scin-Main" }
    , Project { pName = "Scin-Print", pDir = "~/Projects/Scintilla/PrintSys", pApp1 = kitty,     pApp1F = kittyF,     pApp2 = return (), pApp2F = return (), pApp3 = return (), pApp3F = return (), pStart = termBrowSpawn "Scin-Print" }
    , Project { pName = "Scin-Firm",  pDir = "~/Projects/Scintilla/Firmware", pApp1 = kitty,     pApp1F = kittyF,     pApp2 = return (), pApp2F = return (), pApp3 = return (), pApp3F = return (), pStart = termBrowSpawn "Scin-Firm" }
    , Project { pName = "Scin-Docs",  pDir = "~/Projects/Scintilla/docs",     pApp1 = kitty,     pApp1F = kittyF,     pApp2 = return (), pApp2F = return (), pApp3 = return (), pApp3F = return (), pStart = termBrowSpawn "Scin-Docs" }
    , Project { pName = "Scin-Test",  pDir = "~/Projects/Scintilla/Main",     pApp1 = kitty,     pApp1F = kittyF,     pApp2 = scinCont,  pApp2F = return (), pApp3 = return (), pApp3F = return (), pStart = Just $ return () }
    ]
    where
        kitty  = upPointer $ runOrRaiseNext "kitty" (className =? "kitty")
        kittyF = upPointer $ spawn "kitty"

        sameForce r c = (upPointer $ runOrRaiseNext r (className =? c), upPointer $ spawn r)
        (zathura, zathuraF)   = sameForce "zathura" "Zathura"
        (foxit, foxitF)       = sameForce "foxitreader" "Foxit Reader"
        (obsidian, obsidianF) = sameForce "flatpak run org.paraview.Paraview" "ParaView"
        (prusa, prusaF)       = sameForce "flatpak run com.prusa3d.PrusaSlicer" "PrusaSlicer"
        (paraview, paraviewF) = sameForce "flatpak run org.paraview.Paraview" "ParaView"
        (mpv, mpvF)           = sameForce "mpv /home/oleete/Videos/films/*" "mpv"
        (deluge, delugeF)     = sameForce "deluge" "Deluge-gtk"
        (steam, steamF)       = sameForce "steam" "Steam"
        (hdfview, hdfviewF)   = sameForce "hdfview" "SWT"
        scinCont              = upPointer $ raise (title =? "Scintilla Control")

        sl i = "sleep .1; " ++ i
        termBrowSpawn ws = Just $ do spawnOn ws $ sl myTerminal; spawnOn ws ("sleep .5; " ++ myBrowser)
        browSpawn ws = Just $ do spawnOn ws ("sleep .5; " ++ myBrowser)
        oneSpawn ws app = Just $ do spawnOn ws $ sl app
        commentSpawn ws = if n > 1
            then Just $ spawnOn ws $ sl "sleep .4; foxitreader"
            else Just $ do spawnOn ws $ sl myTerminal; spawnOn ws ("sleep .2; " ++ myBrowser); spawnOn ws $ sl "sleep .4; foxitreader"
        filmSpawn ws = Just $ do spawnOn ws $ sl myBrowser; spawnOn ws ("sleep .2; " ++ myTerminal); spawnOn ws $ sl "deluge"

myWorkspaces :: [[Char]]
myWorkspaces = map pName (projects 1)

----------------------------------------------------------------------------------------------------
-- Applications                                                                                   --
----------------------------------------------------------------------------------------------------
myTerminal, myBrowser, myBrowserClass :: [Char]
myTerminal     = "kitty"
myBrowser      = "/home/oleete/.config/bin/browser"
myBrowserClass = "google-chrome-stable"

scratchpads :: [NamedScratchpad]
scratchpads =
    [   NS "gcal" (myBrowserClass ++ " --user-data-dir=/home/oleete/.config/browser/gcal     --class=GCal") (className =? "GCal") nonFloating
    ,   NS "gcalWork" (myBrowserClass ++ " --user-data-dir=/home/oleete/.config/browser/gcalWrk  --class=WrkGCal") (className =? "WrkGCal") nonFloating
    ,   NS "discord"  (myBrowserClass ++ " --user-data-dir=/home/oleete/.config/browser/discord  --class=discord  --app=https://discord.com/channels/@me") (className =? "discord") defaultFloating

    ,   NS "toggl" "flatpak run com.toggl.TogglDesktop" (className =? "Toggl Desktop") nonFloating
    ,   NS "youtubeMusic"  "youtube-music" (className =? "YouTube Music") nonFloating
    ,   NS "calc"  "gnome-calculator" (className =? "gnome-calculator") nonFloating
    ,   NS "console"  "kitty --class=console" (resource =? "console") nonFloating
    ,   NS "ruler"  "kruler" (resource =? "kruler") nonFloating
    ,   NS "sysMon"  "kitty --class=sysMon btop" (resource =? "sysMon") nonFloating
    ]

----------------------------------------------------------------------------------------------------
-- Theme                                                                                          --
----------------------------------------------------------------------------------------------------
background, foreground, dull, active, yellow :: [Char]
background = "#0F0F15"
foreground = "#C8C093"
dull       = "#54546D"
active     = "#7E9CD8"
yellow     = "#DCA561"

-- sizes
gap    = 3
reSize = 1/12
moreReSize = 1/4
myBorder = 0

myWideFont :: [Char]
myWideFont  = "xft:Eurostar Black Extended:" ++ "style=Regular:pixelsize=180:hinting=true"

myShowWNameTheme :: SWNConfig
myShowWNameTheme = def
    { swn_font              = myWideFont
    , swn_fade              = 0.3
    , swn_bgcolor           = active
    , swn_color             = background
    }

myDecoTheme = def
    { inactiveColor = background
    , inactiveBorderColor = background
    , inactiveTextColor = background
    , activeColor = active
    , activeBorderColor = active
    , activeTextColor = active
    , decoHeight = 6
    }

----------------------------------------------------------------------------------------------------
-- Layouts                                                                                        --
----------------------------------------------------------------------------------------------------

mySpacing = spacingRaw False (Border gap gap gap gap) True (Border gap gap gap gap) True

data FULLBAR = FULLBAR deriving (Read, Show, Eq, Typeable)
instance Transformer FULLBAR Window where
    transform FULLBAR x k = k barFull (const x)

barFull :: ModifiedLayout Spacing SimpleFocus a
barFull = mySpacing $ SimpleFocus 1 (reSize/2) 0

data FULLCENTER = FULLCENTER deriving (Read, Show, Eq, Typeable)
instance Transformer FULLCENTER Window where
    transform FULLCENTER x k = k centerFull (const x)

centerFull :: ModifiedLayout Spacing SimpleFocus a
centerFull = mySpacing $ SimpleFocus (1/2) (reSize/2) 600

data TWOPANE = TWOPANE deriving (Read, Show, Eq, Typeable)
instance Transformer TWOPANE Window where
    transform TWOPANE x k = k twoPane (const x)

-- twoPane :: ModifiedLayout Spacing SimpleFocus a
twoPane = mySpacing $ TwoPanePersistent Nothing (reSize) (1/2)

myLayoutHook = smartBorders
             $ mkToggle (single FULL)
             $ mkToggle (single FULLBAR)
             $ mkToggle (single FULLCENTER)
             $ myDeco
             $ draggingVisualizer
             $ mkToggle (single TWOPANE)
             $ mySpacing
               notebookLayout
    where
    myDeco = windowSwitcherDecoration shrinkText myDecoTheme

    notebookMulti   = Notebook True True True 1 2 moreReSize reSize 3 (2/3) 1
    notebookThesis  = Notebook True True True 1 3 moreReSize reSize 2 (2/3) 1
    notebookTwoMain = Notebook False True True 2 3 moreReSize reSize 3 (2/3) 1
    notebookColumns = Notebook False True True 4 4 moreReSize reSize 2 (2/3) 1

    notebookDND = Notebook False True True 4 4 moreReSize reSize 2 (2/3) 0.5

    notebookDifferent = onWorkspaces ["Dnd"] notebookDND $ onWorkspaces ["Thesis", "Print"] notebookThesis $ onWorkspaces ["Comments"] notebookTwoMain notebookMulti

    notebookLaptop = FourTall 1 reSize (2/3)
    notebookLayout = ifWider 1920 (onWorkspaces ["Tmp", "Tmp2", "Home", "Wrk"] notebookColumns notebookDifferent) notebookLaptop

----------------------------------------------------------------------------------------------------
-- Keybindings                                                                                    --
----------------------------------------------------------------------------------------------------
myNav2DConf :: Navigation2DConfig
myNav2DConf = def
    -- { defaultTiledNavigation    = centerNavigation
    -- , floatNavigation           = centerNavigation
    -- , screenNavigation          = lineNavigation
    -- -- , layoutNavigation          = [("Full", centerNavigation)]
    -- , unmappedWindowRect        = [("Full", fullScreenRect)]
    -- }

myModMask :: KeyMask
myModMask = mod4Mask

-- ┏━━━━━━━┳━━━━━━━┳━━━━━━━┳━━━━━━━┳━━━━━━━┓                                   ┏━━━━━━━┳━━━━━━━┳━━━━━━━┳━━━━━━━┳━━━━━━━┓
-- ┃   -   ┃fullWin┃proFind┃cmdPale┃   -   ┃                                   ┃winDown┃winRght┃clseCpy┃ float ┃   -   ┃
-- ┣━━━━━━━╋━━━━━━━╋━━━━━━━╋━━━━━━━╋━━━━━━━┫                                   ┣━━━━━━━╋━━━━━━━╋━━━━━━━╋━━━━━━━╋━━━━━━━┫
-- ┃  ws1  ┃  ws2  ┃  ws3  ┃  ws4  ┃ wsCONF┃                                   ┃winLeft┃  app1 ┃  app2 ┃  app3 ┃  app4 ┃
-- ┣━━━━━━━╋━━━━━━━╋━━━━━━━╋━━━━━━━╋━━━━━━━╋━━━━━━━┳━━━━━━━┓   ┏━━━━━━━┳━━━━━━━╋━━━━━━━╋━━━━━━━╋━━━━━━━╋━━━━━━━╋━━━━━━━┫
-- ┃fullScr┃fullBar┃fullCen┃twoPane┃   -   ┃nspAway┃nextScr┃   ┃   -   ┃  kill ┃ winUp ┃ Master┃ decCol┃ incCol┃   -   ┃
-- ┗━━━━━━━╋━━━━━━━╋━━━━━━━╋━━━━━━━╋━━━━━━━╋━━━━━━━╋━━━━━━━┫   ┣━━━━━━━╋━━━━━━━╋━━━━━━━╋━━━━━━━╋━━━━━━━╋━━━━━━━╋━━━━━━━┛
--         ┃   -   ┃   -   ┃       ┃tabPrev┃ wsLast┃winPrev┃   ┃winNext┃  term ┃tabNext┃       ┃   -   ┃   -   ┃
--         ┗━━━━━━━┻━━━━━━━┛       ┗━━━━━━━┻━━━━━━━━━━━━━━━┛   ┗━━━━━━━┻━━━━━━━┻━━━━━━━┛       ┗━━━━━━━┻━━━━━━━┛
myKeys :: Int -> [(String, X ())]
myKeys n =
    [ ("<XF86MonBrightnessDown>" , spawn "/home/oleete/.config/bin/brightness -dec 5")
    , ("<XF86MonBrightnessUp>"   , spawn "/home/oleete/.config/bin/brightness -inc 5")
    , ("<XF86AudioLowerVolume>"  , spawn "/home/oleete/.config/bin/volume set-sink-volume @DEFAULT_SINK@ -5%" )
    , ("<XF86AudioRaiseVolume>"  , spawn "/home/oleete/.config/bin/volume set-sink-volume @DEFAULT_SINK@ +5%" )
    , ("<XF86AudioMute>"         , spawn "/home/oleete/.config/bin/volume set-sink-volume @DEFAULT_SINK@ 0%" )
    , ("<XF86Display>"           , spawn "/home/oleete/.config/bin/displayctl" )
    , ("<XF86AudioPlay>"         , spawn "playerctl play")
    , ("<XF86AudioStop>"         , spawn "playerctl stop")
    , ("<XF86AudioPause>"        , spawn "playerctl pause")
    , ("<XF86AudioPrev>"         , spawn "playerctl previous")
    , ("<XF86AudioNext>"         , spawn "playerctl next")
    , ("<Print>"                 , spawn "/home/oleete/.config/bin/screencapt area")

    , ("M-p"             , spawn "/home/oleete/.config/bin/rofiScript")
    , ("M-f"             , spawn "/home/oleete/.config/bin/wsHarpoon mainMenu")

    , ("M-<Esc>"         , upPointer $ sequence_ $ hideAllNamedScratchPads scratchpads)


    , ("M-<Return>"      , bF $ nv "OpenTerm" $ l (upPointer $ spawn myTerminal))
    , ("M-S-<Return>"    , upPointer $ spawn myTerminal)

    , ("M-<Backspace>"   , bF $ nv "DeleteBuffer" $ crm (P.sendKey controlMask xK_w) $ l kill)
    , ("M-S-<Backspace>" , kill)

    , ("M-n"             , runProjectApp1)
    , ("M-e"             , runProjectApp2)
    , ("M-i"             , runProjectApp3)
    , ("M-o"             , bF $ crm (P.sendKey controlMask xK_t) $ l (upPointer $ runOrRaise myBrowser (className =? "Google-chrome")))
    , ("M-S-n"           , runProjectApp1Force)
    , ("M-S-e"           , runProjectApp2Force)
    , ("M-S-i"           , runProjectApp3Force)
    , ("M-S-o"           , upPointer $ spawn myBrowser)

    , ("M-<Left>"        , bF $ nv "TabPrev" $ l (P.sendKey (controlMask .|. shiftMask) xK_Tab))
    , ("M-<Right>"       , bF $ nv "TabNext" $ l (P.sendKey controlMask xK_Tab))
    , ("M-<Down>"        , windows W.focusDown)
    , ("M-<Up>"          , windows W.focusUp)


    , ("M-w"             , bF $ nv "ZenOrFull" $ crm (spawn "/home/oleete/.config/bin/chromeFull") $ l (P.sendKey noModMask xK_F11))
    , ("M-z"             , toggleLayout FULL)
    , ("M-x"             , toggleLayout FULLBAR)
    , ("M-c"             , toggleLayout FULLCENTER)
    , ("M-v"             , toggleLayout TWOPANE)

    , ("M-h"             , bF $ nv "Navigateleft"   $ l (upPointer $ windowGo L True))
    , ("M-j"             , bF $ nv "Navigatebottom" $ l (upPointer $ windowGo D True))
    , ("M-k"             , bF $ nv "Navigatetop"    $ l (upPointer $ windowGo U True))
    , ("M-l"             , bF $ nv "Navigateright"  $ l (upPointer $ windowGo R True))
    , ("M-S-h"           , upPointer $ windowSwap L True)
    , ("M-S-j"           , upPointer $ windowSwap D True)
    , ("M-S-k"           , upPointer $ windowSwap U True)
    , ("M-S-l"           , upPointer $ windowSwap R True)

    , ("M-m"             , upPointer $ swapPromote' False)

    , ("M-u"             , killAllOtherCopies)
    , ("M-S-u"           , kill1)
    , ("M-y"             , upPointer $ withFocused toggleFloat)
    , ("M-S-y"           , upFocus sinkAll)

    , ("M-,"             , sendMessage (IncMasterN (-1)))
    , ("M-."             , sendMessage (IncMasterN 1))
    , ("M-S-,"           , sendMessage (IncColumnN (-1)))
    , ("M-S-."           , sendMessage (IncColumnN 1))
    , ("M-["             , sendMessage Shrink)
    , ("M-]"             , sendMessage Expand)
    , ("M-S-["           , sendMessage MirrorShrink)
    , ("M-S-]"           , sendMessage MirrorExpand)
    , ("M-S-9"           , sendMessage SShrink)
    , ("M-S-0"           , sendMessage SExpand)

    , ("M-a"             , spawn "/home/oleete/.config/bin/wsHarpoon jump 1")
    , ("M-r"             , spawn "/home/oleete/.config/bin/wsHarpoon jump 2")
    , ("M-s"             , spawn "/home/oleete/.config/bin/wsHarpoon jump 3")
    , ("M-t"             , spawn "/home/oleete/.config/bin/wsHarpoon jump 4")
    , ("M-S-a"           , spawn "/home/oleete/.config/bin/wsHarpoon move 1")
    , ("M-S-r"           , spawn "/home/oleete/.config/bin/wsHarpoon move 2")
    , ("M-S-s"           , spawn "/home/oleete/.config/bin/wsHarpoon move 3")
    , ("M-S-t"           , spawn "/home/oleete/.config/bin/wsHarpoon move 4")

    , ("M-d"             , spawn "/home/oleete/.config/bin/wsHarpoon jumpName Configs")
    , ("M-S-d"           , spawn "/home/oleete/.config/bin/wsHarpoon moveName Configs")

    , ("M-<Tab>"         , tabCommand)
    , ("M-S-<Tab>"       , shiftTabCommand)
    , ("M-<Space>"       , upFocus $ toggleWS' ["NSP"])
    , ("M-S-<Space>"     , upFocus $ shiftToggleWS' ["NSP"])
    ]
    where
        toggleFloat w = windows (\s -> if M.member w (W.floating s)
                            then W.sink w s
                            else W.float w (W.RationalRect (1/4) (1/4) (1/2) (1/2)) s)
        tabCommand = if n > 1
            then upPointer nextScreen
            else upFocus $ toggleWS' ["NSP"]

        shiftTabCommand = if n > 1
            then upPointer shiftNextScreen
            else upFocus $ shiftToggleWS' ["NSP"]


myMouseBindings :: XConfig l -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings XConfig {} = M.fromList
    [ ((myModMask,               button1) ,\w -> focus w
      >> mouseMoveWindow w
      >> windows W.shiftMaster)
    , ((myModMask,               button3), \w -> focus w
      >> mouseResizeWindow w
      >> windows W.shiftMaster)
    ]

----------------------------------------------------------------------------------------------------
-- Startup                                                                                        --
----------------------------------------------------------------------------------------------------

myStartupHook :: X ()
myStartupHook = do
    killAllStatusBars
    spawn "feh --bg-fill --randomize ~/Pictures/wallpapers/"
    setDefaultCursor xC_left_ptr
    spawnOnce "picom -b --config ~/.config/picom/picom.conf"
    spawnOnce "insync start; insync hide"
    spawnOnce "/home/oleete/.config/bin/startupScript"
    spawnOnce "/home/oleete/.config/bin/connect_screen.py"

----------------------------------------------------------------------------------------------------
-- Log                                                                                            --
----------------------------------------------------------------------------------------------------

barSpawner :: ScreenId -> IO StatusBarConfig
barSpawner (S sid) = pure $
  statusBarPropTo ("_XMONAD_LOG_" ++ show sid) ( "/home/oleete/.config/xmobar/xmobarLaunch " ++ show sid) myPP

myPP = clickablePP $ filterOutWsPP ["NSP"] $ def
    { ppCurrent = xmobarColor active "" . wrap ("<box type=Bottom width=2 mt=2 color=" ++ active ++ ">") "</box>"
    , ppVisible = xmobarColor active ""
    , ppHidden  = xmobarColor dull  ""
    , ppTitle   = xmobarColor foreground "" . wrap ("<box type=Bottom width=2 mt=2 color=" ++ yellow ++ "><fc=" ++ yellow ++ ">") "</fc></box>" . shorten 30
    , ppLayout  = const ""
    , ppSep = xmobarColor foreground "" " | "
    , ppOrder = reverse
    }

myLogHook :: X ()
myLogHook = do
    masterHistoryHook
    workspaceHistoryHookExclude ["NSP"]
    refocusLastLogHook
    showWNameLogHook myShowWNameTheme

----------------------------------------------------------------------------------------------------
-- New Window Actions                                                                             --
----------------------------------------------------------------------------------------------------

myManageHook :: Int -> ManageHook
myManageHook n =
        manageSpecific
    <+> manageDocks
    <+> insertPosition Master Newer
    <+> namedScratchpadManageHook scratchpads
    <+> manageSpawn
    where
        manageSpecific = composeOne
            [ resource  =? "desktop_window"       -?> doIgnore
            , resource  =? "prusa-slicer"         -?> doSink <+> insertPosition End Newer
            , resource  =? "stalonetray"          -?> doIgnore

            , title =? "Scintilla Control"        -?> scinTestShift
            , title =? "Scintilla Variable Editor"-?> doRectFloat halfNhalf
            , title =? "Scintilla Strategy Editor"-?> doRectFloat bigFloat

            , resource  =? "pavucontrol"          -?> doRectFloat (W.RationalRect (8/1920) (31/1080) (600/1920) (800/1080))
            , className =? "Nm-connection-editor" -?> doRectFloat (W.RationalRect (8/1920) (31/1080) (600/1920) (800/1080))
            , className =? "Nm-applet"            -?> doRectFloat (W.RationalRect (8/1920) (31/1080) (600/1920) (800/1080))
            , className =? "Tlp-UI"               -?> doRectFloat (W.RationalRect (8/1920) (31/1080) (600/1920) (800/1080))
            , className =? "Blueberry.py"         -?> doRectFloat (W.RationalRect (8/1920) (31/1080) (600/1920) (800/1080))

            , resource  =? "gnome-calculator"     -?> doCenterFloat
            , className =? "GCal"                 -?> doRectFloat bigFloat
            , className =? "WrkGCal"              -?> doRectFloat bigFloat
            , className =? "Toggl Desktop"        -?> doRectFloat (W.RationalRect (3 / 8) (1 / 8) (1 / 4) (3 / 4))
            , resource  =? "sysMon"               -?> doRectFloat (W.RationalRect (1 / 8) (1 / 8) (3 / 4) (3 / 4))
            , resource  =? "wsHarpoon"            -?> doRectFloat (W.RationalRect (1 / 4) (1 / 4) (2 / 4) (2 / 4))
            , resource  =? "console"              -?> doRectFloat (W.RationalRect (4 / 7) (4 / 7) (2 / 5) (2 / 5))
            , resource  =? "youtube music"        -?> doRectFloat halfNhalf
            , className =? "discord"              -?> doRectFloat halfNhalf
            , resource  =? "kruler"               -?> doFloat

            , transience
            , isBrowserDialog -?> doCenterFloat
            , isRole =? "GtkFileChooserDialog" -?> doCenterFloat
            , isRole =? "pop-up" -?> doCenterFloat
            , isInProperty "_NET_WM_WINDOW_TYPE"
                           "_NET_WM_WINDOW_TYPE_SPLASH" -?> doCenterFloat
            , isFullscreen -?> doFullFloat
            , fmap not isDialog -?> insertPosition End Newer
            ]
        isBrowserDialog = isDialog <&&> className =? myBrowserClass
        halfNhalf = W.RationalRect (1/4) (1/4) (1/2) (1/2)
        bigFloat = W.RationalRect (1/8) (1/8) (3/4) (3/4)
        scinTestShift = if n > 1
            then doShift "Scin-Test"
            else doSink <+> insertPosition End Newer

----------------------------------------------------------------------------------------------------
-- HangleEventHook                                                                                --
----------------------------------------------------------------------------------------------------

myHandleEventHook :: Int -> Event -> X All
myHandleEventHook n = handleEventHook def
                <+> XMonad.Util.Hacks.windowedFullscreenFixEventHook
                <+> myServerModeEventHook n

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
toggleLayout :: (Transformer t a, Typeable a) => t -> X ()
toggleLayout layout = sequence_ [ withFocused $ windows . W.sink, sendMessage $ XMonad.Layout.MultiToggle.Toggle layout, focusUnderPointer ]

-- app bindings
l :: Applicative f => b -> [(f Bool, b)]
l raw = [(pure True, raw)] -- leftover
nv :: MonadIO m => [Char] -> [(Query Bool, m ())] -> [(Query Bool, m ())]
nv command list = (title ~? "Neovim_", spawn ("/home/oleete/.config/bin/nvrWS " ++ command)) : list -- neovim
crm :: b -> [(Query Bool, b)] -> [(Query Bool, b)]
crm raw list = (isRole =? "browser", raw) : list -- chrome

bF :: [(Query Bool, X ())] -> X ()
bF = bindFirst

----------------------------------------------------------------------------------------------------
-- Server Commands                                                                                --
----------------------------------------------------------------------------------------------------

myServerModeEventHook :: Int -> Event -> X All
myServerModeEventHook n = serverModeEventHookCmd' $ return (myCommands' n)

myCommands' :: Int -> [(String, X ())]
myCommands' n = myCommands n ++ sendTo ++ swapTo ++ copyTo
    where sendTo = zipM "move-to-" nums (withNthWorkspace W.shift)
          swapTo = zipM "jump-to-" nums (withNthWorkspace W.greedyView)
          copyTo = zipM "copy-to-" nums (withNthWorkspace copy)
          nums = [0..(length myWorkspaces)]
          zipM  m ks f = zipWith (\k d -> (m ++ show k, upFocus $ f d)) ks ks

myCommands :: Int -> [(String, X ())]
myCommands _ =
    [ ("togglework"          , toggleWS' ["NSP"])
    , ("winGo-h"             , upPointer $ windowGo L True)
    , ("winGo-j"             , upPointer $ windowGo D True)
    , ("winGo-k"             , upPointer $ windowGo U True)
    , ("winGo-l"             , upPointer $ windowGo R True)
    , ("winGo-main"          , upPointer $ swapPromote' False)

    , ("nsp-calc"            , upPointer $ namedScratchpadAction scratchpads "calc")
    , ("nsp-cons"            , upPointer $ namedScratchpadAction scratchpads "console")
    , ("nsp-rulr"            , upPointer $ namedScratchpadAction scratchpads "ruler")
    , ("nsp-disc"            , upPointer $ namedScratchpadAction scratchpads "discord")
    , ("nsp-musc"            , upPointer $ namedScratchpadAction scratchpads "youtubeMusic")
    , ("nsp-sysm"            , upPointer $ namedScratchpadAction scratchpads "sysMon")
    , ("nsp-time"            , upPointer $ namedScratchpadAction scratchpads "toggl")

    , ("nsp-gcal"            , upPointer $ namedScratchpadAction scratchpads "gcal")
    , ("nsp-gcal-wrk"        , upPointer $ namedScratchpadAction scratchpads "gcalWork")

    , ("layout-full"         , toggleLayout FULL)
    , ("layout-fullbar"      , toggleLayout FULLBAR)
    , ("layout-fullcentre"   , toggleLayout FULLCENTER)
    , ("layout-twopane"      , toggleLayout TWOPANE)
    , ("layout-dir"          , upFocus $ sendMessage ToggleSide)
    , ("layout-stack-dir"    , upFocus $ sendMessage ToggleStackDir)
    , ("layout-style"        , upFocus $ sendMessage ToggleMiddle)
    , ("layout-stack-shrink" , sendMessage SShrink)
    , ("layout-stack-expand" , sendMessage SExpand)

    , ("sendF"               , P.sendKey noModMask xK_f)
    , ("sendF11"             , P.sendKey noModMask xK_F11)

    , ("dump-stack"          , debugStack)
    , ("dump-full-stack"     , debugStackFull)

    , ("kill-others"         , killAllOtherCopies)
    , ("kill-self"           , kill1)
    ]
