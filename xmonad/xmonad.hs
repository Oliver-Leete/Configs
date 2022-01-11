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
import qualified Data.Map as M
import Data.Monoid
import System.Exit
import Graphics.X11.Types

import XMonad hiding ( (|||) )
import qualified XMonad.StackSet as W

import XMonad.Actions.ConditionalKeys as C
import XMonad.Actions.CycleWSLocal
import XMonad.Actions.DynamicProjects
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.Navigation2D
import XMonad.Actions.PerWindowKeys
import XMonad.Actions.SinkAll
import XMonad.Actions.SpawnOn
import XMonad.Actions.SwapPromote
import XMonad.Actions.UpdateFocus
import XMonad.Actions.UpdatePointer
import XMonad.Actions.WindowGoLocal

import XMonad.Hooks.EwmhDesktops ( ewmh )
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.Notebook
import XMonad.Layout.PerScreen
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Renamed
import XMonad.Layout.ShowWName
import XMonad.Layout.SimpleFocus
import XMonad.Layout.Spacing

import XMonad.Util.ClickableWorkspaces
import XMonad.Util.EZConfig
import XMonad.Util.Hacks
import XMonad.Util.NamedScratchpad
import XMonad.Util.Paste as P
import XMonad.Util.SpawnOnce

import Data.Tree
import XMonad.Actions.TreeSelect

import XMonad.Prompt.ConfirmPrompt (confirmPrompt)
import XMonad.Actions.WithAll (killAll)
import XMonad.Prompt
----------------------------------------------------------------------------------------------------
-- Main                                                                                           --
----------------------------------------------------------------------------------------------------
main :: IO ()
main = do
    xmonad
        $ dynamicProjects projects
        $ withNavigation2DConfig myNav2DConf
        $ withSB mySB
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
wsPRO1   = "Print"
wsPRO2   = "Dnd"
wsCON    = "Configs"
wsPER    = "Home"
wsWRK    = "Wrk"
wsSIM    = "Sim wrk"
wsTHESIS = "PhD wrk"
wsEXP    = "Exp wrk"

myWorkspaces :: [[Char]]
-- myWorkspaces = [wsTMP, wsPRO1, wsPRO2, wsCON, wsPER, wsWRK, wsSIM, wsEXP, wsTHESIS, wsTMP2]
myWorkspaces = [wsTMP, wsPER, wsPRO1, wsCON, wsPRO2, wsEXP, wsSIM, wsTHESIS, wsWRK, wsTMP2]

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
    , Project   { projectName       = wsPRO1
                , projectDirectory  = "~/Projects/Printing"
                , projectStartHook  = Just $ do spawnOn wsPRO1 "prusa-slicer"
                }
    , Project   { projectName       = wsPRO2
                , projectDirectory  = "~/Projects/Rpgs"
                , projectStartHook  = Just $ do spawnOn wsPRO2 (myLongBrowser ++ " --new-window 'https://roll20.net/welcome'")
                }
    , Project   { projectName       = wsCON
                , projectDirectory  = "~/.config"
                , projectStartHook  = Just $ do spawnOn wsCON (myTerminal ++ " --session=/home/oleete/.config/kitty/config.conf")
                                                spawnOn wsCON ("sleep .5; " ++ myBrowser)
                }
    , Project   { projectName       = wsPER
                , projectDirectory  = "~/PersonalDrive"
                , projectStartHook  = Just $ do spawnOn wsPER (myLongBrowser ++ " --new-window 'github.com' 'feedly.com/i/latest' 'youtube.com/feed/subscriptions' 'coinbase.com'")
                }
    , Project   { projectName       = wsWRK
                , projectDirectory  = "~/UniDrive"
                , projectStartHook  = Just $ do spawnOn wsWRK (myLongBrowser ++ "-wrk --new-window 'sheffield.ac.uk' 'gmail.com' 'calendar.google.com' "
                                                               ++ "'chrome-extension://ndbaejgcaecffnhlmdghchfehkflgfkj/index.html' 'keep.google.com' "
                                                               ++ "'drive.google.com' 'github.com/Oliver-Leete/ThesisLatex' 'feedly.com/i/latest' 'paperpile.com/app'")
                }
    , Project   { projectName       = wsSIM
                , projectDirectory  = "~/Projects/PowderModel"
                , projectStartHook  = Just $ do spawnOn wsSIM (myTerminal ++ " --session=/home/oleete/.config/kitty/sim.conf")
                                                spawnOn wsSIM ("sleep .2; " ++ myBrowser)
                }
    , Project   { projectName       = wsEXP
                , projectDirectory  = "~/Projects/JuliaPlotting"
                , projectStartHook  = Just $ do spawnOn wsEXP (myTerminal ++ " --session=/home/oleete/.config/kitty/exp.conf")
                                                spawnOn wsEXP ("sleep .2; " ++ myBrowser)
                }
    , Project   { projectName       = wsTHESIS
                , projectDirectory  = "~/Projects/Thesis"
                , projectStartHook  = Just $ do spawnOn wsTHESIS (myTerminal ++ " --session=/home/oleete/.config/kitty/thesis.conf")
                                                spawnOn wsTHESIS ("sleep .2; " ++ myBrowser)
                }
    ]

----------------------------------------------------------------------------------------------------
-- Applications                                                                                   --
----------------------------------------------------------------------------------------------------
myTerminal     = "kitty --single-instance --listen-on unix:/tmp/mykitty"
myTerminalRemote = "kitty @ --to unix:/tmp/mykitty launch --type=background"
myBrowser      = "/home/oleete/.config/bin/browser"
myLongBrowser  = "google-chrome-stable --user-data-dir=/home/oleete/.config/browser/google-chrome-stable"
myBrowserClass = "google-chrome-stable"

discordCommand         = "discord --no-sandbox"
gTasksCommand          = myBrowser ++ " '-tasks --app=chrome-extension://ndbaejgcaecffnhlmdghchfehkflgfkj/index.html --class=Tasks'"
gTasksWrkCommand       = myBrowser ++ " 'tasks --app=chrome-extension://ndbaejgcaecffnhlmdghchfehkflgfkj/index.html --class=WrkTasks'"
keepCommand            = myBrowser ++ " '-keep --app=https://keep.google.com/#home --class=Keep'"
keepWrkCommand         = myBrowser ++ " 'keep --app=https://keep.google.com/#home --class=WrkKeep'"
youtubeMusicCommand    = "youtube-music"

scratchpads :: [NamedScratchpad]
scratchpads =
    [   NS "tasks" gTasksCommand (className =? "Tasks") nonFloating
    ,   NS "tasksWork"  gTasksWrkCommand (className =? "WrkTasks") nonFloating
    ,   NS "keepNsp" keepCommand (className =? "Keep") nonFloating
    ,   NS "keepWrkNsp"  keepWrkCommand (className =? "WrkKeep") nonFloating

    ,   NS "discord"  discordCommand (className =? "discord") defaultFloating
    ,   NS "youtubeMusic"  youtubeMusicCommand (className =? "YouTube Music") nonFloating
    ,   NS "calc"  "gnome-calculator --class=calcu" (className =? "calcu") nonFloating
    ,   NS "console"  "alacritty --class console" (resource =? "console") nonFloating
    ,   NS "sysMon"  "alacritty --class sysMon -e btop" (resource =? "sysMon") nonFloating
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
----------------------------------------------------------------------------------------------------
-- Layouts                                                                                        --
----------------------------------------------------------------------------------------------------
mySpacing = spacingRaw False (Border gap gap gap gap) True (Border gap gap gap gap) True

data FULLBAR = FULLBAR deriving (Read, Show, Eq, Typeable)
instance Transformer FULLBAR Window where
    transform FULLBAR x k = k barFull (const x)

barFull = renamed [Replace "Tabs"] $ mySpacing
        $ SimpleFocus 1 (reSize/2) 0

data FULLCENTER = FULLCENTER deriving (Read, Show, Eq, Typeable)
instance Transformer FULLCENTER Window where
    transform FULLCENTER x k = k centerFull (const x)

centerFull = renamed [Replace "Tabs"] $ mySpacing
           $ SimpleFocus (1/3) (reSize/2) 1280

myLayoutHook= smartBorders
            $ mkToggle (single FULL)
            $ mkToggle (single FULLBAR)
            $ mkToggle (single FULLCENTER)
            $ renamed [Replace "Notebook"]
            $ showWName' myShowWNameTheme
            $ mySpacing
              notebookLayout
    where
    notebookMulti   = Notebook 1000  True True True 1 2 reSize 2 (2/3)
    notebookThesis  = Notebook 1000  True True True 1 3 reSize 2 (2/3)
    notebookColumns = Notebook 1000 False True True 4 4 reSize 2 (2/3)
    notebookLaptop = Notebook 1000 True False False 1 2 reSize 2 (2/3)
    notebookLayout = onWorkspaces [wsTMP, wsTMP2, wsPER, wsWRK] notebookColumns
                   $ ifWider 1920 (onWorkspaces [wsTHESIS, wsPRO1] notebookThesis notebookMulti) notebookLaptop

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

myKeys :: [(String, X ())]
myKeys =
    [ ("M-q"                , spawn "xmonad --restart")
    , ("M-M1-q"             , spawn "cd /home/oleete/.config/xmonad; stack install; xmonad --recompile; xmonad --restart; cd -")
    , ("M-S-q"              , confirmPrompt myPromptTheme "Quit XMonad" $ io exitSuccess)

    , ("M-w"                , myTree myTreeConf)
    , ("<XF86MonBrightnessDown>"  , spawn "/home/oleete/.config/bin/brightness -dec 5")
    , ("<XF86MonBrightnessUp>"    , spawn "/home/oleete/.config/bin/brightness -inc 5")
    , ("<XF86AudioLowerVolume>"   , spawn "/home/oleete/.config/bin/volume set-sink-volume @DEFAULT_SINK@ -5%" )
    , ("<XF86AudioRaiseVolume>"   , spawn "/home/oleete/.config/bin/volume set-sink-volume @DEFAULT_SINK@ +5%" )
    , ("<XF86AudioMute>"          , spawn "/home/oleete/.config/bin/volume set-sink-volume @DEFAULT_SINK@ 0%" )
    , ("<XF86Display>"            , spawn "/home/oleete/.config/bin/displayctl" )
    , ("<XF86AudioPlay>"          , spawn "playerctl play-pause")
    , ("<XF86AudioStop>"          , spawn "playerctl play-pause")
    , ("<XF86AudioPause>"         , spawn "playerctl play-pause")
    , ("<XF86AudioPrev>"          , spawn "playerctl previous")
    , ("<XF86AudioNext>"          , spawn "playerctl next")
    , ("<Print>"                  , spawn "/home/oleete/.config/bin/screencapt area")
    , ("M-<Esc>"                  , upPointer $ allNamedScratchpadAction scratchpads "sysMon")
    , ("M-S-<Esc>"                , upPointer $ allNamedScratchpadAction scratchpads "sysMon")

    , ("M-<Return>"         , kittyBind2 (P.sendKey (controlMask .|. shiftMask) xK_Return) (upPointer $ runOrRaise myTerminal (className =? "kitty")))
    , ("M-M1-<Return>"      , upPointer $ spawn myTerminal)

    , ("M-b"                , chromeBind (P.sendKey controlMask xK_t) (upPointer $ runOrRaise myBrowser (className =? "Google-chrome")))
    , ("M-M1-b"             , upPointer $ spawn myBrowser)
    , ("M-S-b"              , upPointer altBrowser)

    , ("M-a"                , spawn "rofi -matching fuzzy -modi combi -show combi -combi-modi window,drun,run -show-icons")
    , ("M-v"                , upPointer $ runOrRaise "zathura" (className =? "Zathura"))

    , ("M-S-c"              , upPointer $ allNamedScratchpadAction scratchpads "calc")
    , ("M-S-<Return>"       , upPointer $ allNamedScratchpadAction scratchpads "console")
    , ("M-S-d"              , upPointer $ allNamedScratchpadAction scratchpads "discord")
    , ("M-S-m"              , upPointer $ allNamedScratchpadAction scratchpads "youtubeMusic")
    , ("M-S-t"              , upPointer $ wrkNSP "tasksWork" "tasks")
    , ("M-S-n"              , upPointer $ wrkNSP "keepWrkNsp" "keepNsp")

    , ("M-<Backspace>"      , multiBind (spawn (myTerminalRemote ++ " delWindow")) (P.sendKey controlMask xK_w) kill)
    , ("M-M1-<Backspace>"   , kill   )
    , ("M-<Delete>"         , confirmPrompt myPromptTheme "kill all" killAll)

    , ("M-<Down>"           , multiBind (spawn (myTerminalRemote ++ " tabSwap Right Down")) (P.sendKey controlMask xK_Tab) (bindOn LD [("Tabs", windows W.focusDown)]))
    , ("M-<Up>"             , multiBind (spawn (myTerminalRemote ++ " tabSwap Left Up")) (P.sendKey (controlMask .|. shiftMask) xK_Tab) (bindOn LD [("Tabs", windows W.focusUp)]))
    , ("M-M1-<Down>"        , bindOn LD [("Tabs", windows W.focusDown)])
    , ("M-M1-<Up>"          , bindOn LD [("Tabs", windows W.focusUp)])
    , ("M-t"                , multiBind (P.sendKey (controlMask .|. shiftMask) xK_t) (P.sendKey controlMask xK_t) (spawn ""))

    , ("M-f"                , kittyBind " kittyFullscreen"  (toggleLayout FULL))
    , ("M-M1-f"             , toggleLayout FULL)
    , ("M-M1-S-f"           , toggleLayout FULL)
    , ("M-s"                , toggleLayout FULLBAR)
    , ("M-c"                , toggleLayout FULLCENTER)

    , ("M-e"                , upPointer $ sequence_ [raise (className =? "kitty"), spawn (myTerminalRemote ++ " focusEditor")])

    , ("M-h"                , kittyBind " moveWindow left h"   (upPointer $ windowGo L True))
    , ("M-j"                , kittyBind " moveWindow bottom j" (upPointer $ windowGo D True))
    , ("M-k"                , kittyBind " moveWindow top k"    (upPointer $ windowGo U True))
    , ("M-l"                , kittyBind " moveWindow right l"  (upPointer $ windowGo R True))
    , ("M-M1-S-h"           , upPointer $ windowGo L True)
    , ("M-M1-S-j"           , upPointer $ windowGo D True)
    , ("M-M1-S-k"           , upPointer $ windowGo U True)
    , ("M-M1-S-l"           , upPointer $ windowGo R True)
    , ("M-M1-h"             , upPointer $ windowSwap L True)
    , ("M-M1-j"             , upPointer $ windowSwap D True)
    , ("M-M1-k"             , upPointer $ windowSwap U True)
    , ("M-M1-l"             , upPointer $ windowSwap R True)

    , ("M-m"                , kittyBind " mainMove"   (upPointer $ swapPromote' False))
    , ("M-M1-m"             , upPointer $ swapPromote' False)
    , ("M-M1-S-m"           , upPointer $ swapPromote' False)
    , ("M-y"                , upPointer $ withFocused toggleFloat)
    , ("M-M1-y"             , upFocus sinkAll)
    -- , ("M-h"                , bindFirst [(title =? "MainEditor", spawn "nvr --servername /tmp/nvr-server-1 --nostart -c 'KittyNavigateleft'")
    --                         ,(className =? "kitty", spawn (myTerminalRemote ++ " moveWindow left h"))
    --                         ,(pure True,upPointer $ windowGo L True)])

    , ("M-,"                , sendMessage (IncMasterN (-1)))
    , ("M-."                , sendMessage (IncMasterN 1))
    , ("M-M1-,"             , sendMessage (IncColumnN (-1)))
    , ("M-M1-."             , sendMessage (IncColumnN 1))
    , ("M-["                , sendMessage Shrink)
    , ("M-]"                , sendMessage Expand)
    , ("M-M1-["             , sendMessage MirrorShrink)
    , ("M-M1-]"             , sendMessage MirrorExpand)

    , ("M-r"                , upFocus $ sendMessage ToggleSide)
    , ("M-M1-r"             , upFocus $ sendMessage ToggleStackDir)
    , ("M-x"                , upFocus $ sendMessage ToggleMiddle)

    , ("M-<Space>"          , upFocus $ toggleWS' ["NSP"])
    , ("M-M1-<Space>"       , upFocus $ shiftToggleWS' ["NSP"])
    ]
    ++ zipM "M-"            wsKeys [0..] (withNthWorkspace W.greedyView)
    ++ zipM "M-M1-"         wsKeys [0..] (withNthWorkspace W.shift)
    where
        upFocus a = sequence_ [a, focusUnderPointer]
        upPointer a = sequence_ [a, updatePointer (0.5, 0.5) (0.25, 0.25)]

        wsKeys = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0"]
        zipM  m ks as f = zipWith (\k d -> (m ++ k, upFocus $ f d)) ks as

        toggleFloat w = windows (\s -> if M.member w (W.floating s)
                            then W.sink w s
                            else W.float w (W.RationalRect (1/4) (1/4) (1/2) (1/2)) s)

        altBrowser = bindOn C.WS [(wsTMP2,   spawn myLongBrowser) ,(wsTMP,   spawn myLongBrowser)
                               ,(wsWRK,   spawn myLongBrowser) ,(wsTHESIS,spawn myLongBrowser)
                               ,(wsEXP,   spawn myLongBrowser) ,(wsSIM,   spawn myLongBrowser)
                               ,("",      spawn (myLongBrowser ++ "-wrk"))]

        wrkNSP work personal = bindOn C.WS [(wsWRK, allNamedScratchpadAction scratchpads work)
                                         ,(wsSIM, allNamedScratchpadAction scratchpads work)
                                         ,(wsEXP, allNamedScratchpadAction scratchpads work)
                                         ,(wsTHESIS, allNamedScratchpadAction scratchpads work)
                                         ,("", allNamedScratchpadAction scratchpads personal)]

        kittyBind kitty leftover = bindFirst [(className =? "kitty", spawn (myTerminalRemote ++ kitty))
                                          ,(pure True, leftover)]

        kittyBind2 kitty leftover = bindFirst [(className =? "kitty", kitty)
                                               ,(pure True, leftover)]

        chromeBind chrome leftover = bindFirst [(className =? "Google-chrome", chrome)
                                               ,(pure True, leftover)]

        multiBind kitty chrome leftover = bindFirst [(className =? "kitty", kitty)
                                                    ,(className =? "Google-chrome", chrome)
                                                    ,(pure True, leftover)]

        toggleLayout layout = sequence_ [ withFocused $ windows . W.sink, sendMessage $ XMonad.Layout.MultiToggle.Toggle layout, focusUnderPointer ]

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
    killStatusBar "xmobar ~/.config/xmobar/xmobar.conf"
    spawnStatusBar "xmobar ~/.config/xmobar/xmobar.conf"
    spawn "feh --bg-fill --randomize ~/Pictures/wallpapers/"
    spawnOnce "xsetroot -cursor_name left_ptr"
    spawnOnce "picom -b --config ~/.config/picom/picom.conf"
    spawnOnce "insync start; insync hide"
    spawnOnce "/home/oleete/.config/bin/startupScript"
    spawnOnce "/home/oleete/.config/bin/connect_screen.py"

----------------------------------------------------------------------------------------------------
-- Log                                                                                            --
----------------------------------------------------------------------------------------------------

mySB = statusBarProp "xmobar" (clickablePP $ filterOutWsPP ["NSP"] myPP)
myPP = def
    { ppCurrent = xmobarColor active "" . wrap ("<box type=Bottom width=2 mt=2 color=" ++ active ++ ">") "</box>"
    , ppVisible = xmobarColor active ""
    , ppHidden  = xmobarColor dull  ""
    , ppTitle   = xmobarColor foreground "" . wrap ("<box type=Bottom width=2 mt=2 color=" ++ yellow ++ "><fc=" ++ yellow ++ ">") "</fc></box>" . shorten 30
    , ppLayout  = const ""
    , ppSep = xmobarColor foreground "" " | "
    , ppOrder = reverse
    }

myLogHook = do
    fadeWindowsLogHook myFadeHook
    masterHistoryHook

myFadeHook :: FadeHook
myFadeHook = composeAll
    [ opaque
    , isUnfocused --> opacity 0.9
    , isDialog --> opaque
    ]

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
            [ resource =? "desktop_window" -?> doIgnore
            , resource =? "prusa-slicer" -?> doSink <+> insertPosition End Newer
            , resource =? "stalonetray"    -?> doIgnore

            , resource =? "pavucontrol" -?> doRectFloat (W.RationalRect (8/1920) (31/1080) (600/1920) (800/1080))
            , className =? "Nm-connection-editor" -?> doRectFloat (W.RationalRect (8/1920) (31/1080) (600/1920) (800/1080))
            , className =? "Nm-applet" -?> doRectFloat (W.RationalRect (8/1920) (31/1080) (600/1920) (800/1080))
            , className =? "Tlp-UI" -?> doRectFloat (W.RationalRect (8/1920) (31/1080) (600/1920) (800/1080))
            , className =? "Blueberry.py" -?> doRectFloat (W.RationalRect (8/1920) (31/1080) (600/1920) (800/1080))
            , className =? "Insync" -?> doRectFloat (W.RationalRect (8/1920) (31/1080) (600/1920) (800/1080))
            , className =? "Volctl" -?> doRectFloat (W.RationalRect (8/1920) (31/1080) (600/1920) (800/1080))

            , resource =? "galculator" -?> doCenterFloat
            , resource =? "Tasks" -?> doRectFloat halfNhalf
            , resource =? "WrkTasks" -?> doRectFloat halfNhalf
            , className =? "Keep" -?> doRectFloat halfNhalf
            , className =? "WrkKeep" -?> doRectFloat halfNhalf
            , resource =? "sysMon" -?> doRectFloat (W.RationalRect (1 / 8) (1 / 8) (3 / 4) (3 / 4))
            , resource =? "console" -?> doRectFloat (W.RationalRect (4 / 7) (4 / 7) (2 / 5) (2 / 5))
            , resource =? "youtube music" -?> doRectFloat halfNhalf
            , resource =? "discord" -?> doRectFloat halfNhalf

            , transience
            -- , isBrowserDialog -?> forceCenterFloat
            -- , isRole =? "GtkFileChooserDialog" -?> forceCenterFloat
            , isBrowserDialog -?> doCenterFloat
            , isRole =? "GtkFileChooserDialog" -?> doCenterFloat
            , isRole =? "pop-up" -?> doCenterFloat
            , isInProperty "_NET_WM_WINDOW_TYPE"
                           "_NET_WM_WINDOW_TYPE_SPLASH" -?> doCenterFloat
            , isFullscreen -?> doFullFloat
            , fmap not isDialog -?> insertPosition End Newer
            ]
        isBrowserDialog = isDialog <&&> className =? myBrowserClass
        isRole = stringProperty "WM_WINDOW_ROLE"
        halfNhalf = W.RationalRect (1/4) (1/4) (1/2) (1/2)

----------------------------------------------------------------------------------------------------
-- X Event Actions                                                                                --
----------------------------------------------------------------------------------------------------

myHandleEventHook :: Event -> X All
myHandleEventHook = fadeWindowsEventHook
                <+> handleEventHook def
                <+> XMonad.Util.Hacks.windowedFullscreenFixEventHook

----------------------------------------------------------------------------------------------------
-- Tree Menu                                                                                      --
----------------------------------------------------------------------------------------------------

myTreeConf = TSConfig { ts_hidechildren = True
           , ts_background   = 0x00000000
           , ts_font         = "xft:Sans-16"
           , ts_node         = (0xffd0d0d0, 0xff1a1b26)
           , ts_nodealt      = (0xffd0d0d0, 0xff1a1b26)
           , ts_highlight    = (0xffffffff, 0xff755999)
           , ts_extra        = 0x00000000
           , ts_node_width   = 200
           , ts_node_height  = 30
           , ts_originX      = 20
           , ts_originY      = 30
           , ts_indent       = 80
           , ts_navigate     = myTreeNav
           }

myTreeNav = M.fromList
    [ ((0, xK_Escape), cancel)
    , ((mod4Mask, xK_w), cancel)
    , ((0, xK_Return), select)
    , ((0, xK_space),  select)
    , ((0, xK_Up),     movePrev)
    , ((0, xK_Down),   moveNext)
    , ((0, xK_Left),   moveParent)
    , ((0, xK_Right),  moveChild)
    , ((0, xK_k),      movePrev)
    , ((0, xK_j),      moveNext)
    , ((0, xK_h),      moveParent)
    , ((0, xK_l),      moveChild)
    ]

myTree a = treeselectAction a
   [ Node (TSNode "Lock"       "Lock the session"     (spawn "slock")) []
   , Node (TSNode "Media"  "Media Controls" (return ()))
       [ Node (TSNode "Play/Pause"     "Play/pause media"       (spawn "playerctl play-pause")) []
       , Node (TSNode "Next Track"     "Skip to next track"     (spawn "playerctl next"))  []
       , Node (TSNode "Previous Track" "Skip to previous track" (spawn "playerctl previous"))  []
       ]
   , Node (TSNode "Utilities"  "All that useful stuff" (return ()))
       [ Node (TSNode "color picker"    "Find that color" (spawn "/home/oleete/.config/bin/colorPicker")) []
       , Node (TSNode "Calculator"      "For maths and shit" (spawn "galculator")) []
       , Node (TSNode "Screenkey"       "Show key presses" (spawn "killall screenkey || screenkey"))  []
       , Node (TSNode "Screenshot"      "Take a screenshot" (spawn "/home/oleete/.config/bin/screencapt"))  []
       , Node (TSNode "Screenshot Area" "Take a screenshot" (spawn "/home/oleete/.config/bin/screencapt area"))  []
       , Node (TSNode "Screencap"       "Take a screen capture" (spawn "/home/oleete/.config/bin/screencast"))  []
       , Node (TSNode "Screencap Area"  "Take a screen capture" (spawn "/home/oleete/.config/bin/screencast area"))  []
       , Node (TSNode "System Monitor"  "Monitor the system" (allNamedScratchpadAction scratchpads "sysMon")) []
       ]
   , Node (TSNode "Brightness" "Sets screen brightness using xbacklight" (return ()))
       [ Node (TSNode "Bright" "FULL POWER!!"            (spawn "/home/oleete/.config/bin/brightness -set 100")) []
       , Node (TSNode "High"   "Normal Brightness (75%)" (spawn "/home/oleete/.config/bin/brightness -set 75"))  []
       , Node (TSNode "Normal" "Normal Brightness (50%)" (spawn "/home/oleete/.config/bin/brightness -set 50"))  []
       , Node (TSNode "Low"    "Normal Brightness (25%)" (spawn "/home/oleete/.config/bin/brightness -set 25"))  []
       , Node (TSNode "Dim"    "Quite dark"              (spawn "/home/oleete/.config/bin/brightness -set 10"))  []
       ]
   , Node (TSNode "Display" "Set the display mode" (spawn "/home/oleete/.config/bin/displayctl"))
       [ Node (TSNode "Internal" "Internal display only"     (spawn "/home/oleete/.config/bin/displayctl internal")) []
       , Node (TSNode "External" "External display only"     (spawn "/home/oleete/.config/bin/displayctl external"))  []
       , Node (TSNode "Mirror"   "Mirro display"             (spawn "/home/oleete/.config/bin/displayctl mirror"))  []
       , Node (TSNode "Span"     "Span all displays"         (spawn "/home/oleete/.config/bin/displayctl span"))  []
       , Node (TSNode "SpanEx"   "Span external displays"    (spawn "/home/oleete/.config/bin/displayctl spanex"))  []
       ]
   , Node (TSNode "Networks"    "Select networks" (spawn "networkmanager_dmenu")) []
   , Node (TSNode "Settings" "Open settings" (return ()))
       [ Node (TSNode "Volume"  "Manage audio devices" (spawn "pavucontrol")) []
       , Node (TSNode "Bluetooth"  "Manage bluetooth devices" (spawn "blueberry")) []
       , Node (TSNode "Network Settings"    "Manage networks" (spawn "nm-connection-editor")) []
       , Node (TSNode "Battery"    "Manage power settings" (spawn "tlp-ui")) []
       , Node (TSNode "Screenkey"  "Screenkey settings" (spawn "screenkey --show-settings"))  []
       ]
   , Node (TSNode "Power" "Suspend or Shutdown" (return ()))
       [ Node (TSNode "Logout"     "Logout of session" (io exitSuccess)) []
       , Node (TSNode "Suspend"    "Sleep the system" (spawn "systemctl suspend")) []
       , Node (TSNode "Hibernate"  "Hibernate the system" (spawn "systemctl hibernate")) []
       , Node (TSNode "Shutdown"   "Poweroff the system" (spawn "systemctl poweroff")) []
       , Node (TSNode "Reboot"     "Reboot the system" (spawn "systemctl reboot")) []
       ]
   ]
