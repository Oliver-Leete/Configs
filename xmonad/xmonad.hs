{-# LANGUAGE AllowAmbiguousTypes, DeriveDataTypeable, TypeSynonymInstances, MultiParamTypeClasses #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
import qualified Data.Map as M
import Data.Monoid
import Graphics.X11.Types

import XMonad hiding ( (|||) )
import qualified XMonad.StackSet as W

import XMonad.Actions.CycleWS (nextScreen, shiftNextScreen)
import XMonad.Actions.CycleWSLocal
import XMonad.Actions.DynamicProjectsLocal
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.Navigation2D
import XMonad.Actions.PerWindowKeys
import XMonad.Actions.SpawnOn
import XMonad.Actions.SwapPromote
import XMonad.Actions.UpdateFocus
import XMonad.Actions.UpdatePointer
import XMonad.Actions.WindowGoLocal
import XMonad.Actions.WithAll (sinkAll)

import XMonad.Hooks.DebugStack
import XMonad.Hooks.EwmhDesktops ( ewmh )
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.RefocusLast
import XMonad.Hooks.ServerMode
import XMonad.Hooks.ShowWName
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.WorkspaceHistory (workspaceHistoryHookExclude)

import XMonad.Layout.FourColumns
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.Notebook
import XMonad.Layout.PerScreen
import XMonad.Layout.PerWorkspace
import XMonad.Layout.SimpleFocus
import XMonad.Layout.Spacing

import XMonad.Util.ClickableWorkspaces
import XMonad.Util.Cursor
import XMonad.Util.EZConfig
import XMonad.Util.Hacks
import XMonad.Util.NamedScratchpadLocal
import XMonad.Util.Paste as P
import XMonad.Util.SpawnOnce

----------------------------------------------------------------------------------------------------
-- Main                                                                                           --
----------------------------------------------------------------------------------------------------
main :: IO ()
main = do
    xmonad
        $ dynamicProjects projects
        $ withNavigation2DConfig myNav2DConf
        $ dynamicSBs barSpawner
        $ ewmh
        $ docks myConfig

myConfig = def
        { borderWidth        = myBorder
        , clickJustFocuses   = True
        , focusFollowsMouse  = True
        , normalBorderColor  = background
        , focusedBorderColor = active
        , manageHook         = myManageHook
        , handleEventHook    = myHandleEventHook
        , layoutHook         = myLayoutHook
        , logHook            = myLogHook
        , modMask            = myModMask
        , mouseBindings      = myMouseBindings
        , startupHook        = myStartupHook
        , terminal           = myTerminal
        , workspaces         = myWorkspaces
        } `additionalKeysP` myKeys


----------------------------------------------------------------------------------------------------
-- Workspaces                                                                                     --
----------------------------------------------------------------------------------------------------

projects :: [Project]
projects =
    [ Project { pName = "Tmp",       pDir = "/tmp",                          pApp1 = kitty,    pApp1F = kittyForce,    pApp2 = return (), pApp2F = return (),     pApp3 = return (), pApp3F = return (),   pStart = Just $ return () }
    , Project { pName = "Tmp2",      pDir = "/tmp",                          pApp1 = kitty,    pApp1F = kittyForce,    pApp2 = return (), pApp2F = return (),     pApp3 = return (), pApp3F = return (),   pStart = Just $ return () }

    , Project { pName = "Home",      pDir = "~/PersonalDrive",               pApp1 = kitty,    pApp1F = kittyForce,    pApp2 = zathura,   pApp2F = zathuraForce,  pApp3 = return (), pApp3F = return (),   pStart = browSpawn "Home" }
    , Project { pName = "CodeTuts",  pDir = "~/Projects/rustBook",           pApp1 = nvim,     pApp1F = nvimForce,     pApp2 = return (), pApp2F = return (),     pApp3 = return (), pApp3F = return (),   pStart = termBrowSpawn "CodeTuts" }
    , Project { pName = "Print",     pDir = "~/Projects/Printing",           pApp1 = prusa,    pApp1F = prusaForce,    pApp2 = return (), pApp2F = return (),     pApp3 = return (), pApp3F = return (),   pStart = oneSpawn "Print" "flatpak run com.prusa3d.PrusaSlicer" }
    , Project { pName = "Game",      pDir = "~/Documents",                   pApp1 = steam,    pApp1F = steamForce,    pApp2 = return (), pApp2F = return (),     pApp3 = return (), pApp3F = return (),   pStart = oneSpawn "Game" "steam" }
    , Project { pName = "Film",      pDir = "~/Videos/films",                pApp1 = kitty,    pApp1F = kittyForce,    pApp2 = mpv,       pApp2F = mpvForce,      pApp3 = deluge,    pApp3F = delugeForce, pStart = filmSpawn "Film" }
    , Project { pName = "DND",       pDir = "~/Projects/Rpgs",               pApp1 = kitty,    pApp1F = kittyForce,    pApp2 = zathura,   pApp2F = zathuraForce,  pApp3 = return (), pApp3F = return (),   pStart = browSpawn "DND" }

    , Project { pName = "Configs",   pDir = "~/.config",                     pApp1 = nvim,     pApp1F = nvimForce,     pApp2 = return (), pApp2F = return (),     pApp3 = return (), pApp3F = return (),   pStart = termBrowSpawn "Configs" }
    , Project { pName = "QMK",       pDir = "~/Projects/qmk_firmware",       pApp1 = nvim,     pApp1F = nvimForce,     pApp2 = return (), pApp2F = return (),     pApp3 = return (), pApp3F = return (),   pStart = termBrowSpawn "QMK" }
    , Project { pName = "ZMK",       pDir = "~/Projects/zmk-config",         pApp1 = nvim,     pApp1F = nvimForce,     pApp2 = return (), pApp2F = return (),     pApp3 = return (), pApp3F = return (),   pStart = termBrowSpawn "ZMK" }

    , Project { pName = "Work",      pDir = "~/UniDrive",                    pApp1 = kitty,    pApp1F = kittyForce,    pApp2 = zathura,   pApp2F = zathuraForce,  pApp3 = return (), pApp3F = return (),   pStart = browSpawn "Work" }
    , Project { pName = "WorkNotes", pDir = "~/Projects/Thesis/Notes",       pApp1 = obsidian, pApp1F = obsidianForce, pApp2 = zathura,   pApp2F = zathuraForce,  pApp3 = return (), pApp3F = return (),   pStart = oneSpawn "WorkNotes" "flatpak run md.obsidian.Obsidian" }
    , Project { pName = "Thesis",    pDir = "~/Projects/Thesis/thesis",      pApp1 = nvim,     pApp1F = nvimForce,     pApp2 = zathura,   pApp2F = zathuraForce,  pApp3 = return (), pApp3F = return (),   pStart = termBrowSpawn "Thesis" }
    , Project { pName = "Sim",       pDir = "~/Projects/PowderModel",        pApp1 = nvim,     pApp1F = nvimForce,     pApp2 = return (), pApp2F = return (),     pApp3 = return (), pApp3F = return (),   pStart = termBrowSpawn "Sim" }
    , Project { pName = "Exp",       pDir = "~/Projects/JuliaPlotting",      pApp1 = nvim,     pApp1F = nvimForce,     pApp2 = return (), pApp2F = return (),     pApp3 = return (), pApp3F = return (),   pStart = termBrowSpawn "Exp" }
    , Project { pName = "Comments",  pDir = "~/Projects/Thesis/thesis",      pApp1 = nvim,     pApp1F = nvimForce,     pApp2 = zathura,   pApp2F = zathuraForce,  pApp3 = foxit,     pApp3F = foxitForce,  pStart = commentSpawn "Comments" }
    , Project { pName = "ANSYS",     pDir = "~/Projects/ANSYSpowderModel",   pApp1 = nvim,     pApp1F = nvimForce,     pApp2 = paraview,  pApp2F = paraviewForce, pApp3 = return (), pApp3F = return (),   pStart = termBrowSpawn "ANSYS" }

    , Project { pName = "Scin-Main",   pDir = "~/Projects/Scintilla/Main",     pApp1 = nvim,     pApp1F = nvimForce,     pApp2 = return (), pApp2F = return (),     pApp3 = return (), pApp3F = return (),   pStart = termBrowSpawn "Scin-Main" }
    , Project { pName = "Scin-Print",   pDir = "~/Projects/Scintilla/PrintSys", pApp1 = nvim,     pApp1F = nvimForce,     pApp2 = return (), pApp2F = return (),     pApp3 = return (), pApp3F = return (),   pStart = termBrowSpawn "Scin-Print" }
    , Project { pName = "Scin-Firm",   pDir = "~/Projects/Scintilla/Firmware", pApp1 = nvim,     pApp1F = nvimForce,     pApp2 = return (), pApp2F = return (),     pApp3 = return (), pApp3F = return (),   pStart = termBrowSpawn "Scin-Firm" }
    , Project { pName = "Scin-Docs",   pDir = "~/Projects/Scintilla/docs",     pApp1 = nvim,     pApp1F = nvimForce,     pApp2 = return (), pApp2F = return (),     pApp3 = return (), pApp3F = return (),   pStart = termBrowSpawn "Scin-Docs" }
    ]
    where
        nvim = bF $ kt " focusEditor" $ l (upPointer $ sequence_ [raise (className =? "kitty"), spawn (myTerminalRemote ++ " focusEditor")])
        nvimForce = upPointer $ spawn (myTerminal ++ " fish -C nvrStart")
        zathura = upPointer $ runOrRaise "zathura" (className =? "Zathura")
        zathuraForce = upPointer $ spawn "zathura"
        kitty = upPointer $ runOrRaise "kittyMaker" (className =? "kitty")
        kittyForce = upPointer $ spawn "kitty"

        foxit = upPointer $ runOrRaise "foxitreader" (className =? "Foxit Reader")
        foxitForce = upPointer $ spawn "foxitreader"
        obsidian = upPointer $ runOrRaise "flatpak run org.paraview.Paraview" (className =? "ParaView")
        obsidianForce = upPointer $ spawn "flatpak run org.paraview.Paraview"
        prusa = upPointer $ runOrRaise "flatpak run com.prusa3d.PrusaSlicer" (className =? "PrusaSlicer")
        prusaForce = upPointer $ spawn "flatpak run com.prusa3d.PrusaSlicer"
        paraview = upPointer $ runOrRaise "flatpak run org.paraview.Paraview" (className =? "ParaView")
        paraviewForce = upPointer $ spawn "flatpak run org.paraview.Paraview"
        mpv = upPointer $ runOrRaise "mpv /home/oleete/Videos/films/*" (className =? "mpv")
        mpvForce = upPointer $ spawn "mpv /home/oleete/Videos/films/*"
        deluge = upPointer $ runOrRaise "deluge" (className =? "Deluge-gtk")
        delugeForce = upPointer $ spawn "deluge"
        steam = upPointer $ runOrRaise "steam" (className =? "Steam")
        steamForce = upPointer $ spawn "steam"

        sl i = "sleep .1; " ++ i
        termBrowSpawn ws = Just $ do spawnOn ws $ sl myTerminal; spawnOn ws ("sleep .5; " ++ myBrowser)
        browSpawn ws = Just $ do spawnOn ws ("sleep .5; " ++ myBrowser)
        oneSpawn ws app = Just $ do spawnOn ws $ sl app
        commentSpawn ws = Just $ do spawnOn ws $ sl myTerminal; spawnOn ws ("sleep .2; " ++ myBrowser); spawnOn ws $ sl "sleep .4; foxitreader"
        filmSpawn ws = Just $ do spawnOn ws $ sl myBrowser; spawnOn ws ("sleep .2; " ++ myTerminal); spawnOn ws $ sl "deluge"

myWorkspaces :: [[Char]]
myWorkspaces = map pName projects

----------------------------------------------------------------------------------------------------
-- Applications                                                                                   --
----------------------------------------------------------------------------------------------------
myTerminal, myTerminalRemote, myBrowser, myBrowserClass :: [Char]
myTerminal     = "/home/oleete/.config/bin/kittyMaker"
myTerminalRemote = "/home/oleete/.config/bin/kittyRemote"
myBrowser      = "/home/oleete/.config/bin/browser"
myBrowserClass = "google-chrome-stable"

scratchpads :: [NamedScratchpad]
scratchpads =
    [   NS "gcal" (myBrowserClass ++ " --user-data-dir=/home/oleete/.config/browser/gcal     --class=GCal") (className =? "GCal") nonFloating
    ,   NS "gcalWork" (myBrowserClass ++ " --user-data-dir=/home/oleete/.config/browser/gcalWrk  --class=WrkGCal") (className =? "WrkGCal") nonFloating
    ,   NS "discord"  (myBrowserClass ++ " --user-data-dir=/home/oleete/.config/browser/discord  --class=discord  --app=https://discord.com/channels/@me") (className =? "discord") defaultFloating

    ,   NS "youtubeMusic"  "youtube-music" (className =? "YouTube Music") nonFloating
    ,   NS "calc"  "gnome-calculator" (className =? "gnome-calculator") nonFloating
    ,   NS "console"  "alacritty --class console" (resource =? "console") nonFloating
    ,   NS "sysMon"  "alacritty --class sysMon -t 'System Monitor' -e btop" (resource =? "sysMon") nonFloating
    ]

----------------------------------------------------------------------------------------------------
-- Theme                                                                                          --
----------------------------------------------------------------------------------------------------
background, foreground, dull, active, yellow :: [Char]
background = "#1F1F28"
foreground = "#C8C093"
dull       = "#54546D"
active     = "#7E9CD8"
yellow     = "#DCA561"

-- sizes
gap    = 3
reSize = 1/10
moreReSize = 1/2
myBorder = 3

myWideFont :: [Char]
myWideFont  = "xft:Eurostar Black Extended:"
            ++ "style=Regular:pixelsize=180:hinting=true"

myShowWNameTheme :: SWNConfig
myShowWNameTheme = def
    { swn_font              = myWideFont
    , swn_fade              = 0.3
    , swn_bgcolor           = active
    , swn_color             = background
    }

----------------------------------------------------------------------------------------------------
-- Layouts                                                                                        --
----------------------------------------------------------------------------------------------------
data FULLBAR = FULLBAR deriving (Read, Show, Eq, Typeable)
instance Transformer FULLBAR Window where
    transform FULLBAR x k = k barFull (const x)

barFull :: SimpleFocus a
barFull = SimpleFocus 1 (reSize/2) 0

data FULLCENTER = FULLCENTER deriving (Read, Show, Eq, Typeable)
instance Transformer FULLCENTER Window where
    transform FULLCENTER x k = k centerFull (const x)

centerFull :: SimpleFocus a
centerFull = SimpleFocus (1/2) (reSize/2) 600

myLayoutHook = smartBorders
             $ mkToggle (single FULL)
             $ spacingRaw False (Border gap gap gap gap) True (Border gap gap gap gap) True
             $ mkToggle (single FULLBAR)
             $ mkToggle (single FULLCENTER)
               notebookLayout
    where
    notebookMulti   = Notebook True True True 1 2 moreReSize reSize 3 (2/3)
    notebookThesis  = Notebook True True True 1 3 moreReSize reSize 2 (2/3)
    notebookTwoMain = Notebook False True True 2 3 moreReSize reSize 3 (2/3)
    notebookColumns = Notebook False True True 4 4 moreReSize reSize 2 (2/3)

    notebookDifferent = onWorkspaces ["Thesis", "Print"] notebookThesis $ onWorkspaces ["Comments"] notebookTwoMain notebookMulti

    notebookLaptop = FourTall 1 reSize (2/3)
    notebookLayout = ifWider 1920 (onWorkspaces ["Tmp", "Tmp2", "Home", "Work"] notebookColumns notebookDifferent) notebookLaptop

----------------------------------------------------------------------------------------------------
-- Keybindings                                                                                    --
----------------------------------------------------------------------------------------------------
myNav2DConf :: Navigation2DConfig
myNav2DConf = def
    { defaultTiledNavigation    = centerNavigation
    , floatNavigation           = centerNavigation
    , screenNavigation          = lineNavigation
    , layoutNavigation          = [("Full", centerNavigation)]
    , unmappedWindowRect        = [("Full", singleWindowRect)]
    }

myModMask :: KeyMask
myModMask = mod4Mask

-- ---------------------------------------------------------------------------------------------------------------------
-- |restart|fullWin| finder|project|   -   |-----------------------------------|winDown|winRght|   -   | float |   -   |
-- |  ws1  |  ws2  |  ws3  |  ws4  | wsCONF|-----------------------------------|winLeft|  app1 |  app2 |  app3 |  app4 |
-- |fullScr|fullBar|fullCen|   -   |   -   |nspAway|nextScr|---|   -   |  kill | winUp | Master| decCol| incCol|   -   |
-- |-------|   -   |   -   |-------|tabPrev| wsLast|winPrev|---|winNext|  term |tabNext|-------|   -   |   -   |-------|
myKeys :: [(String, X ())]
myKeys =
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

    , ("M-q"             , spawn "xmonad --restart")
    , ("M-S-q"           , spawn "cd /home/oleete/.config/xmonad; stack install; xmonad --recompile; xmonad --restart; cd -")

    , ("M-p"             , spawn "/home/oleete/.config/bin/rofiScript")
    , ("M-f"             , spawn "/home/oleete/.config/bin/wsHarpoon mainMenu")

    , ("M-<Esc>"         , upPointer $ sequence_ $ hideAllNamedScratchPads scratchpads)


    , ("M-<Return>"      , bF $ kt " kittyWin" $ l(upPointer $ spawn myTerminal))
    , ("M-S-<Return>"    , upPointer $ spawn myTerminal)

    , ("M-n"             , runProjectApp1)
    , ("M-e"             , runProjectApp2)
    , ("M-i"             , runProjectApp3)
    , ("M-o"             , bF $ crm (P.sendKey controlMask xK_t) $ l (upPointer $ runOrRaise myBrowser (className =? "Google-chrome")))
    , ("M-S-n"           , runProjectApp1Force)
    , ("M-S-e"           , runProjectApp2Force)
    , ("M-S-i"           , runProjectApp3Force)
    , ("M-S-o"           , upPointer $ spawn myBrowser)

    , ("M-<Backspace>"   , bF $ nv "DeleteBuffer" $ rKt (P.sendKey (controlMask .|. mod1Mask) xK_BackSpace) $ crm (P.sendKey controlMask xK_w) $ l kill)
    , ("M-S-<Backspace>" , kill)

    , ("M-<Left>"        , bF $ nv "TabPrev" $ rKt (P.sendKey (controlMask .|. mod1Mask) xK_Left)  $ l (P.sendKey (controlMask .|. shiftMask) xK_Tab))
    , ("M-<Right>"       , bF $ nv "TabNext" $ rKt (P.sendKey (controlMask .|. mod1Mask) xK_Right) $ l (P.sendKey controlMask xK_Tab))
    , ("M-<Down>"        , windows W.focusDown)
    , ("M-<Up>"          , windows W.focusUp)


    , ("M-w"             , bF $ nv "ZenOrFull" $ rKt (P.sendKey (controlMask .|. mod1Mask) xK_f) $ crm (spawn "/home/oleete/.config/bin/chromeFull") $ l (P.sendKey noModMask xK_F11))
    , ("M-z"             , toggleLayout FULL)
    , ("M-x"             , toggleLayout FULLBAR)
    , ("M-c"             , toggleLayout FULLCENTER)

    , ("M-h"             , bF $ nv "KittyNavigateleft"   $ kt " moveWindow left"   $ l (upPointer $ windowGo L True))
    , ("M-j"             , bF $ nv "KittyNavigatebottom" $ kt " moveWindow bottom" $ l (upPointer $ windowGo D True))
    , ("M-k"             , bF $ nv "KittyNavigatetop"    $ kt " moveWindow top"    $ l (upPointer $ windowGo U True))
    , ("M-l"             , bF $ nv "KittyNavigateright"  $ kt " moveWindow right"  $ l (upPointer $ windowGo R True))

    , ("M-S-h"           , upPointer $ windowSwap L True)
    , ("M-S-j"           , upPointer $ windowSwap D True)
    , ("M-S-k"           , upPointer $ windowSwap U True)
    , ("M-S-l"           , upPointer $ windowSwap R True)

    -- , ("M-m"             , myFocusMaster)
    , ("M-m"             , bF $ kt " mainMove" $ l (upPointer $ swapPromote' False))
    , ("M-S-m"           , upPointer $ swapPromote' False)

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

    , ("M-<Tab>"         , upPointer nextScreen)
    , ("M-S-<Tab>"       , upPointer shiftNextScreen)
    , ("M-<Space>"       , upFocus $ toggleWS' ["NSP"])
    , ("M-S-<Space>"     , upFocus $ shiftToggleWS' ["NSP"])
    ]
    where
        toggleFloat w = windows (\s -> if M.member w (W.floating s)
                            then W.sink w s
                            else W.float w (W.RationalRect (1/4) (1/4) (1/2) (1/2)) s)

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

myManageHook :: ManageHook
myManageHook =
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

            , resource  =? "pavucontrol"          -?> doRectFloat (W.RationalRect (8/1920) (31/1080) (600/1920) (800/1080))
            , className =? "Nm-connection-editor" -?> doRectFloat (W.RationalRect (8/1920) (31/1080) (600/1920) (800/1080))
            , className =? "Nm-applet"            -?> doRectFloat (W.RationalRect (8/1920) (31/1080) (600/1920) (800/1080))
            , className =? "Tlp-UI"               -?> doRectFloat (W.RationalRect (8/1920) (31/1080) (600/1920) (800/1080))
            , className =? "Blueberry.py"         -?> doRectFloat (W.RationalRect (8/1920) (31/1080) (600/1920) (800/1080))
            , className =? "Insync"               -?> doRectFloat (W.RationalRect (8/1920) (31/1080) (600/1920) (800/1080))

            , resource  =? "gnome-calculator"     -?> doCenterFloat
            , className =? "GCal"                 -?> doRectFloat bigFloat
            , className =? "WrkGCal"              -?> doRectFloat bigFloat
            , resource  =? "sysMon"               -?> doRectFloat (W.RationalRect (1 / 8) (1 / 8) (3 / 4) (3 / 4))
            , resource  =? "wsHarpoon"            -?> doRectFloat (W.RationalRect (1 / 4) (1 / 4) (2 / 4) (2 / 4))
            , resource  =? "console"              -?> doRectFloat (W.RationalRect (4 / 7) (4 / 7) (2 / 5) (2 / 5))
            , resource  =? "youtube music"        -?> doRectFloat halfNhalf
            , className =? "discord"              -?> doRectFloat halfNhalf

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

----------------------------------------------------------------------------------------------------
-- HangleEventHook                                                                                --
----------------------------------------------------------------------------------------------------

myHandleEventHook :: Event -> X All
myHandleEventHook = handleEventHook def
                <+> XMonad.Util.Hacks.windowedFullscreenFixEventHook
                <+> myServerModeEventHook

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
nv command list = (title =? "MainEditor", spawn ("/home/oleete/.config/bin/nvrWS " ++ command)) : list -- neovim
kt :: MonadIO m => [Char] -> [(Query Bool, m ())] -> [(Query Bool, m ())]
kt command list = (className =? "kitty", spawn (myTerminalRemote ++ command)) : list -- kitty
rKt :: b -> [(Query Bool, b)] -> [(Query Bool, b)]
rKt raw list = (className =? "kitty", raw) : list -- kitty raw
crm :: b -> [(Query Bool, b)] -> [(Query Bool, b)]
crm raw list = (isRole =? "browser", raw) : list -- chrome

bF :: [(Query Bool, X ())] -> X ()
bF = bindFirst

----------------------------------------------------------------------------------------------------
-- Server Commands                                                                                --
----------------------------------------------------------------------------------------------------

myServerModeEventHook :: Event -> X All
myServerModeEventHook = serverModeEventHookCmd' $ return myCommands'

myCommands' :: [(String, X ())]
myCommands' = myCommands ++ sendTo ++ swapTo
    where sendTo = zipM "move-to-" nums (withNthWorkspace W.shift)
          swapTo = zipM "jump-to-" nums (withNthWorkspace W.greedyView)
          nums = [0..(length myWorkspaces)]
          zipM  m ks f = zipWith (\k d -> (m ++ show k, upFocus $ f d)) ks ks

myCommands :: [(String, X ())]
myCommands =
    [ ("togglework"          , toggleWS' ["NSP"])
    , ("winGo-left"          , upPointer $ windowGo L True)
    , ("winGo-bottom"        , upPointer $ windowGo D True)
    , ("winGo-top"           , upPointer $ windowGo U True)
    , ("winGo-right"         , upPointer $ windowGo R True)
    , ("winGo-main"          , upPointer $ swapPromote' False)

    , ("nsp-calc"            , upPointer $ namedScratchpadAction scratchpads "calc")
    , ("nsp-cons"            , upPointer $ namedScratchpadAction scratchpads "console")
    , ("nsp-disc"            , upPointer $ namedScratchpadAction scratchpads "discord")
    , ("nsp-musc"            , upPointer $ namedScratchpadAction scratchpads "youtubeMusic")
    , ("nsp-sysm"            , upPointer $ namedScratchpadAction scratchpads "sysMon")

    , ("nsp-gcal"            , upPointer $ namedScratchpadAction scratchpads "gcal")
    , ("nsp-gcal-wrk"        , upPointer $ namedScratchpadAction scratchpads "gcalWork")

    , ("layout-full"         , toggleLayout FULL)
    , ("layout-fullbar"      , toggleLayout FULLBAR)
    , ("layout-fullcentre"   , toggleLayout FULLCENTER)
    , ("layout-dir"          , upFocus $ sendMessage ToggleSide)
    , ("layout-stack-dir"    , upFocus $ sendMessage ToggleStackDir)
    , ("layout-style"        , upFocus $ sendMessage ToggleMiddle)

    , ("sendF"               , P.sendKey noModMask xK_f)
    , ("sendF11"             , P.sendKey noModMask xK_F11)

    , ("dump-stack"          , debugStack)
    , ("dump-full-stack"     , debugStackFull)
    ]
