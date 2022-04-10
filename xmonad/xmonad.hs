{-# LANGUAGE AllowAmbiguousTypes, DeriveDataTypeable, TypeSynonymInstances, MultiParamTypeClasses #-}
----------------------------------------------------------------------------------------------------
--                                                                                                --
--                        __   __  __  __                               _                         --
--                        \ \ / / |  \/  |                             | |                        --
--                         \ V /  | \  / |   ___    _ __     __ _    __| |                        --
--                          > <   | |\/| |  / _ \  | '_ \   / _` |  / _` |                        --
--                         / . \  | |  | | | (_) | | | | | | (_| | | (_| |                        --
--                        /_/ \_\ |_|  |_|  \___/  |_| |_|  \__,_|  \__,_|                        --
--                                                                                                --
----------------------------------------------------------------------------------------------------
-- Oliver Leete <oliverleete@gmail.com>                                                           --
-- https://github.com/oliver-leete                                                                --
----------------------------------------------------------------------------------------------------
-- Originally based on Ethan Schoonover's config Since been massively butchered                   --

----------------------------------------------------------------------------------------------------
-- Modules                                                                                        --
----------------------------------------------------------------------------------------------------

{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE FlexibleContexts #-}
import qualified Data.Map as M
import Data.Monoid
import System.Exit
import Graphics.X11.Types

import XMonad hiding ( (|||) )
import qualified XMonad.StackSet as W

import XMonad.Actions.CycleWSLocal
import XMonad.Actions.CycleWS (nextScreen, shiftNextScreen)
import XMonad.Actions.DynamicProjects
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.Navigation2D
import XMonad.Actions.PerWindowKeys
import XMonad.Actions.PerWorkspaceKeys
import XMonad.Actions.SinkAll
import XMonad.Actions.SpawnOn
import XMonad.Actions.SwapPromote
import XMonad.Actions.UpdateFocus
import XMonad.Actions.UpdatePointer
import XMonad.Actions.WindowGoLocal

import XMonad.Hooks.EwmhDesktops ( ewmh )
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ServerMode
import XMonad.Hooks.ShowWName
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.StatusBar.WorkspaceScreen

import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.Notebook
import XMonad.Layout.PerScreen
import XMonad.Layout.PerWorkspace
import XMonad.Layout.SimpleFocus
import XMonad.Layout.Spacing

import XMonad.Util.EZConfig
import XMonad.Util.Hacks
import XMonad.Util.NamedScratchpadLocal
import XMonad.Util.Paste as P
import XMonad.Util.SpawnOnce

import XMonad.Prompt.ConfirmPrompt (confirmPrompt)
import XMonad.Actions.WithAll (killAll)
import XMonad.Prompt
import XMonad.Hooks.RefocusLast
import XMonad.Prelude
import XMonad.Hooks.WorkspaceHistory (workspaceHistoryHookExclude)
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
        , normalBorderColor  = borderCol
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
wsTMP    = "Tmp"
wsTMP2   = "Tmp2"
wsPER    = "Home"
wsPER1   = "Home1"
ws3D     = "Print"
wsDND    = "Dnd"
wsCON    = "Configs"
wsQMK    = "QMK"
wsZMK    = "ZMK"
wsWRK    = "Wrk"
wsWRK1   = "Wrk1"
wsSIM    = "Sim"
wsTHESIS = "Thesis"
wsEXP    = "Exp"
wsANSYS  = "ANSYS"
wsCOMMENTS = "Comments"
wsWRKN   = "WrkNotes"
wsSCN    = "Scintilla"
wsSCNN   = "Scin-Docs"
wsCODE   = "CodeTuts"
wsGAME   = "Games"

myWorkspaces :: [[Char]]
myWorkspaces = [wsTMP, wsTMP2, wsPER1, ws3D, wsDND, wsCON, wsPER, wsWRK, wsEXP, wsSIM, wsTHESIS, wsWRK1, wsQMK, wsANSYS, wsCOMMENTS, wsWRKN, wsZMK, wsSCN, wsCODE, wsGAME, wsSCNN]

projects :: [Project]
projects =

    [ Project   { projectName       = wsTMP
                , projectDirectory  = "/tmp"
                , projectStartHook  = Just $ return ()
                }
    , Project   { projectName       = wsTMP2
                , projectDirectory  = "/tmp"
                , projectStartHook  = Just $ return ()
                }
    , Project   { projectName       = ws3D
                , projectDirectory  = "~/Projects/Printing"
                , projectStartHook  = Just $ do spawnOn ws3D "prusa-slicer"
                }
    , Project   { projectName       = wsDND
                , projectDirectory  = "~/Projects/Rpgs"
                , projectStartHook  = Just $ spawnOn wsDND myBrowser
                }
    , Project   { projectName       = wsCON
                , projectDirectory  = "~/.config"
                , projectStartHook  = Just $ do spawnOn wsCON myTerminal
                                                spawnOn wsCON ("sleep .5; " ++ myBrowser)
                }
    , Project   { projectName       = wsQMK
                , projectDirectory  = "~/Projects/qmk_firmware"
                , projectStartHook  = Just $ do spawnOn wsQMK myTerminal
                                                spawnOn wsQMK ("sleep .5; " ++ myBrowser)
                }
    , Project   { projectName       = wsZMK
                , projectDirectory  = "~/Projects/zmk-config"
                , projectStartHook  = Just $ do spawnOn wsZMK myTerminal
                                                spawnOn wsZMK ("sleep .5; " ++ myBrowser)
                }
    , Project   { projectName       = wsPER
                , projectDirectory  = "~/PersonalDrive"
                , projectStartHook  = Just $ do spawnOn wsPER myBrowser
                }
    , Project   { projectName       = wsWRK
                , projectDirectory  = "~/UniDrive"
                , projectStartHook  = Just $ do spawnOn wsWRK myBrowser
                }
    , Project   { projectName       = wsSIM
                , projectDirectory  = "~/Projects/PowderModel"
                , projectStartHook  = Just $ do spawnOn wsSIM myTerminal
                                                spawnOn wsSIM ("sleep .2; " ++ myBrowser)
                }
    , Project   { projectName       = wsEXP
                , projectDirectory  = "~/Projects/JuliaPlotting"
                , projectStartHook  = Just $ do spawnOn wsEXP myTerminal
                                                spawnOn wsEXP ("sleep .2; " ++ myBrowser)
                }
    , Project   { projectName       = wsTHESIS
                , projectDirectory  = "~/Projects/Thesis/thesis"
                , projectStartHook  = Just $ do spawnOn wsTHESIS myTerminal
                                                spawnOn wsTHESIS ("sleep .2; " ++ myBrowser)
                }
    , Project   { projectName       = wsCOMMENTS
                , projectDirectory  = "~/Projects/Thesis/thesis"
                , projectStartHook  = Just $ do spawnOn wsCOMMENTS myTerminal
                                                spawnOn wsCOMMENTS ("sleep .2; " ++ myBrowser)
                                                spawnOn wsCOMMENTS "sleep .4; foxitreader"
                }
    , Project   { projectName       = wsWRKN
                , projectDirectory  = "~/Projects/Thesis/Notes"
                , projectStartHook  = Just $ do spawnOn wsCOMMENTS "obsidian"
                }
    , Project   { projectName       = wsANSYS
                , projectDirectory  = "~/Projects/ANSYSpowderModel"
                , projectStartHook  = Just $ do spawnOn wsANSYS myTerminal
                                                spawnOn wsANSYS ("sleep .2; " ++ myBrowser)
                }
    , Project   { projectName       = wsCODE
                , projectDirectory  = "~/Projects/rustBook"
                , projectStartHook  = Just $ do spawnOn wsCODE myTerminal
                                                spawnOn wsCODE ("sleep .2; " ++ myBrowser)
                }
    , Project   { projectName       = wsSCN
                , projectDirectory  = "~/Projects/Scintilla/Scintilla"
                , projectStartHook  = Just $ do spawnOn wsSCN myTerminal
                                                spawnOn wsSCN ("sleep .2; " ++ myBrowser)
                }
    , Project   { projectName       = wsSCNN
                , projectDirectory  = "~/Projects/Scintilla/docs"
                , projectStartHook  = Just $ do spawnOn wsSCNN myTerminal
                                                spawnOn wsSCNN ("sleep .2; " ++ myBrowser)
                }
    , Project   { projectName       = wsGAME
                , projectDirectory  = "~/Documents"
                , projectStartHook  = Just $ do spawnOn wsGAME "steam"
                }
    ]

----------------------------------------------------------------------------------------------------
-- Applications                                                                                   --
----------------------------------------------------------------------------------------------------
myTerminal     = "/home/oleete/.config/bin/kittyMaker"
myTerminalRemote = "/home/oleete/.config/bin/kittyRemote"
myBrowser      = "/home/oleete/.config/bin/browser"
myBrowserClass = "google-chrome-stable"

discordCommand   = myBrowserClass ++ " --user-data-dir=/home/oleete/.config/browser/discord  --class=discord  --app=https://discord.com/channels/@me"
gcalCommand      = myBrowserClass ++ " --user-data-dir=/home/oleete/.config/browser/gcal     --class=GCal"
gcalCommandWrk   = myBrowserClass ++ " --user-data-dir=/home/oleete/.config/browser/gcalWrk  --class=WrkGCal"
ytmCommand       = "youtube-music"

scratchpads :: [NamedScratchpad]
scratchpads =
    [   NS "gcal" gcalCommand (className =? "GCal") nonFloating
    ,   NS "gcalWork" gcalCommandWrk (className =? "WrkGCal") nonFloating

    ,   NS "discord"  discordCommand (className =? "discord") defaultFloating
    ,   NS "youtubeMusic"  ytmCommand (className =? "YouTube Music") nonFloating
    ,   NS "calc"  "gnome-calculator" (className =? "gnome-calculator") nonFloating
    ,   NS "console"  "alacritty --class console" (resource =? "console") nonFloating
    ,   NS "sysMon"  "alacritty --class sysMon -t 'System Monitor' -e btop" (resource =? "sysMon") nonFloating
    ]

----------------------------------------------------------------------------------------------------
-- Theme                                                                                     --
----------------------------------------------------------------------------------------------------
borderCol  = "#1a1b26"
background = "#1F1F28"
foreground = "#C8C093"
dull       = "#54546D"
active     = "#76946A"
yellow     = "#DCA561"
warning    = "#C34043"

-- sizes
gap    = 4
reSize = 1/20
myBorder = 3

myWideFont  = "xft:Eurostar Black Extended:"
            ++ "style=Regular:pixelsize=180:hinting=true"


myShowWNameTheme :: SWNConfig
myShowWNameTheme = def
    { swn_font              = myWideFont
    , swn_fade              = 0.3
    , swn_bgcolor           = active
    , swn_color             = background
    }

myPromptTheme :: XPConfig
myPromptTheme = def
    { bgColor               = active
    , fgColor               = background
    , fgHLight              = dull
    , bgHLight              = active
    , borderColor           = active
    , promptBorderWidth     = 0
    , height                = 30
    , position              = CenteredAt (1 / 4) (1 / 4)
    , autoComplete          = Nothing
    }

hotPromptTheme :: XPConfig
hotPromptTheme = myPromptTheme
    { bgColor               = warning
    , fgColor               = background
    }
----------------------------------------------------------------------------------------------------
-- Layouts                                                                                        --
----------------------------------------------------------------------------------------------------
data FULLBAR = FULLBAR deriving (Read, Show, Eq, Typeable)
instance Transformer FULLBAR Window where
    transform FULLBAR x k = k barFull (const x)

barFull = SimpleFocus 1 (reSize/2) 0

data FULLCENTER = FULLCENTER deriving (Read, Show, Eq, Typeable)
instance Transformer FULLCENTER Window where
    transform FULLCENTER x k = k centerFull (const x)

centerFull = SimpleFocus (1/3) (reSize/2) 1280

myLayoutHook= smartBorders
            $ mkToggle (single FULL)
            $ spacingRaw False (Border gap gap gap gap) True (Border gap gap gap gap) True
            $ mkToggle (single FULLBAR)
            $ mkToggle (single FULLCENTER)
              notebookLayout
    where
    notebookMulti   = Notebook True True True 1 2 reSize 2 (2/3)
    notebookThesis  = Notebook True True True 1 3 reSize 2 (2/3)
    notebookColumns = Notebook False True True 4 4 reSize 2 (2/3)
    notebookLaptop = Notebook True False False 1 2 reSize 2 (2/3)
    notebookTwoMain = Notebook False True True 2 3 reSize 2 (2/3)
    notebookDifferent = onWorkspaces [wsTHESIS, ws3D] notebookThesis $ onWorkspaces [wsCOMMENTS] notebookTwoMain notebookMulti
    notebookLayout = onWorkspaces [wsTMP, wsTMP2, wsPER, wsWRK] notebookColumns
                   $ ifWider 1920 notebookDifferent notebookLaptop

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

myModMask = mod4Mask

-- ---------------------------------------------------------------------------------------------------------------------
-- |restart|fullWin| finder|project|   -   |-----------------------------------|winDown|winRght|   -   | float |   -   |
-- |  ws1  |  ws2  |  ws3  |  ws4  |   -   |-----------------------------------|winLeft|  app1 |  app2 |  app3 |  app4 |
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
    , ("M-C-q"           , confirmPrompt myPromptTheme "Quit XMonad" $ io exitSuccess)

    , ("M-p"             , spawn "/home/oleete/.config/bin/rofiScript")
    , ("M-f"             , spawn "/home/oleete/.config/bin/wsHarpoon mainMenu")

    , ("M-<Esc>"         , upPointer $ sequence_ $ hideAllNamedScratchPads scratchpads)


    , ("M-<Return>"      , bF $ kt " kittyWin" $ l(upPointer $ spawn myTerminal))
    , ("M-S-<Return>"    , upPointer $ spawn myTerminal)

    , ("M-n"             , bF $ rNv (spawn (myTerminalRemote ++ " kittyWin")) $ kt " focusEditor" $ l (upPointer $ sequence_ [raise (className =? "kitty"), spawn (myTerminalRemote ++ " focusEditor")]))
    , ("M-S-n"           , upPointer $ spawn myTerminal)

    , ("M-i"             , bF $ crm (P.sendKey controlMask xK_t) $ l (upPointer $ runOrRaise myBrowser (className =? "Google-chrome")))
    , ("M-S-i"           , upPointer $ spawn myBrowser)

    , ("M-e"             , upPointer $ runOrRaise "zathura" (className =? "Zathura"))

    , ("M-o"             , upPointer $ bindOn [(wsCOMMENTS, runOrRaise "foxitreader" (className =? "Foxit Reader"))])

    , ("M-<Backspace>"   , bF $ nv "DeleteBuffer" $ rKt (P.sendKey (controlMask .|. mod1Mask) xK_BackSpace) $ crm (P.sendKey controlMask xK_w) $ l kill)
    , ("M-S-<Backspace>" , kill)
    , ("M-C-<Backspace>" , confirmPrompt hotPromptTheme "kill all" killAll)

    , ("M-<Left>"        , bF $ rKt (P.sendKey (controlMask .|. mod1Mask) xK_Left)  $ l (P.sendKey (controlMask .|. shiftMask) xK_Tab))
    , ("M-<Right>"       , bF $ rKt (P.sendKey (controlMask .|. mod1Mask) xK_Right) $ l (P.sendKey controlMask xK_Tab))
    , ("M-<Down>"        , windows W.focusDown)
    , ("M-<Up>"          , windows W.focusUp)


    , ("M-w"             , bF $ rKt (P.sendKey (controlMask .|. mod1Mask) xK_f) $ crm (spawn "/home/oleete/.config/bin/chromeFull") $ l (P.sendKey noModMask xK_F11))
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

    , ("M-m"             , myFocusMaster)
    , ("M-S-m"           , bF $ kt " mainMove" $ l (upPointer $ swapPromote' False))

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

    , ("M-<Tab>"         , upPointer nextScreen)
    , ("M-S-<Tab>"       , upPointer shiftNextScreen)
    , ("M-<Space>"       , upFocus $ toggleWS' ["NSP"])
    , ("M-S-<Space>"     , upFocus $ shiftToggleWS' ["NSP"])
    ]
    where
        toggleFloat w = windows (\s -> if M.member w (W.floating s)
                            then W.sink w s
                            else W.float w (W.RationalRect (1/4) (1/4) (1/2) (1/2)) s)

        l leftover = [(pure True, leftover)]
        nv n o = (title =? "MainEditor", spawn ("/home/oleete/.config/bin/nvrWS " ++ n)) : o
        rNv n o = (title =? "MainEditor", n) : o
        kt k o = (className =? "kitty", spawn (myTerminalRemote ++ k)) : o
        rKt k o = (className =? "kitty", k) : o
        crm c o = (isRole =? "browser", c) : o

        bF o = bindFirst o

myFocusMaster :: X ()
myFocusMaster = withWindowSet $ \wset ->
  case W.index wset of
    []      -> pure ()
    (x : _) -> if   Just x == W.peek wset
               then toggleFocus
               else windows W.focusMaster

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
    spawn "xsetroot -cursor_name left_ptr"
    spawnOnce "picom -b --config ~/.config/picom/picom.conf"
    spawnOnce "insync start; insync hide"
    spawnOnce "/home/oleete/.config/bin/startupScript"
    spawnOnce "/home/oleete/.config/bin/connect_screen.py"

----------------------------------------------------------------------------------------------------
-- Log                                                                                            --
----------------------------------------------------------------------------------------------------

monitorIds :: IO [(ScreenId, String)]
monitorIds = return [(S 0, "¹"), (S 1, "²"), (S 2, "³"), (S 3, "⁴")]

myScreenCombiner :: X WorkspaceScreenCombiner
myScreenCombiner = do
  screenNames <- io monitorIds
  return
    $ \w sc -> w <> fromJust (W.screen sc `lookup` screenNames)

barSpawner :: ScreenId -> IO StatusBarConfig
barSpawner (S sid) = pure $
  statusBarPropTo ("_XMONAD_LOG_" ++ show sid) ( "/home/oleete/.config/xmobar/xmobarLaunch " ++ show sid) myPP

myPP = combineWithScreen myScreenCombiner $ filterOutWsPP ["NSP"] $ def
    { ppCurrent = xmobarColor active "" . wrap ("<box type=Bottom width=2 mt=2 color=" ++ active ++ ">") "</box>"
    , ppVisible = xmobarColor active ""
    , ppHidden  = xmobarColor dull  ""
    , ppTitle   = xmobarColor foreground "" . wrap ("<box type=Bottom width=2 mt=2 color=" ++ yellow ++ "><fc=" ++ yellow ++ ">") "</fc></box>" . shorten 30
    , ppLayout  = const ""
    , ppSep = xmobarColor foreground "" " | "
    , ppOrder = reverse
    }

myLogHook = do
    masterHistoryHook
    workspaceHistoryHookExclude ["NSP"]
    -- nsHideOnFocusLoss scratchpads
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
        -- thirdNthird = W.RationalRect (1/5) (1/5) (3/5) (3/5)
        bigFloat = W.RationalRect (1/8) (1/8) (3/4) (3/4)

----------------------------------------------------------------------------------------------------
-- HangleEventHook                                                                                --
----------------------------------------------------------------------------------------------------

myHandleEventHook :: Event -> X All
myHandleEventHook = handleEventHook def
                <+> XMonad.Util.Hacks.windowedFullscreenFixEventHook
                <+> myServerModeEventHook
                -- <+> serverModeEventHookF "XMONAD_PRINT" (io . putStrLn)

----------------------------------------------------------------------------------------------------
-- Helper Functions                                                                               --
----------------------------------------------------------------------------------------------------

isRole :: Query String
isRole = stringProperty "WM_WINDOW_ROLE"

upFocus :: X () -> X ()
upFocus a = sequence_ [a, focusUnderPointer]

upPointer :: X () -> X ()
upPointer a = sequence_ [a, updatePointer (0.5, 0.5) (0.25, 0.25)]

toggleLayout layout = sequence_ [ withFocused $ windows . W.sink, sendMessage $ XMonad.Layout.MultiToggle.Toggle layout, focusUnderPointer ]

----------------------------------------------------------------------------------------------------
-- Server Commands                                                                                --
----------------------------------------------------------------------------------------------------

myServerModeEventHook = serverModeEventHookCmd' $ return myCommands'
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
    ]
