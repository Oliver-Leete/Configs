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
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import System.Exit
import System.IO
import Graphics.X11.Types

import XMonad hiding ( (|||) )
import qualified XMonad.StackSet as W

import XMonad.Actions.ConditionalKeys
import XMonad.Actions.CycleWSLocal
import XMonad.Actions.DynamicProjects
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.EasyMotion
import XMonad.Actions.Navigation2D
import XMonad.Actions.PerWindowKeys
import XMonad.Actions.SinkAll
import XMonad.Actions.SpawnOn
import XMonad.Actions.SwapPromote
import XMonad.Actions.WindowGoLocal
import XMonad.Actions.WithAll

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops ( ewmhDesktopsLogHook, ewmh )
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.Notebook
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Renamed
import XMonad.Layout.ShowWName
import XMonad.Layout.SimpleFocus
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed

import XMonad.Prompt
import XMonad.Prompt.ConfirmPrompt
import XMonad.Prompt.FuzzyMatch

import XMonad.Util.EZConfig
import XMonad.Util.Hacks
import XMonad.Util.NamedScratchpad
import XMonad.Util.Paste as P
import XMonad.Util.Run
import XMonad.Util.SpawnOnce


----------------------------------------------------------------------------------------------------
-- Main                                                                                           --
----------------------------------------------------------------------------------------------------
main :: IO ()
main = do
    xmproc0 <- spawnPipe "xmobar -x0 $HOME/.config/xmobar/xmobar.conf"

    xmonad
        $ dynamicProjects projects
        $ withNavigation2DConfig myNav2DConf
        $ ewmh
        $ myConfig xmproc0

myConfig p = def
        { borderWidth        = myBorder
        , clickJustFocuses   = True
        , focusFollowsMouse  = True
        , normalBorderColor  = background
        , focusedBorderColor = active
        , manageHook         = myManageHook
        , handleEventHook    = myHandleEventHook
        , layoutHook         = myLayoutHook
        , logHook            = myLogHook p
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
wsPRO2   = "Notes"
wsPRO3   = "Dnd"
wsCON    = "Configs"
wsPER    = "Home"
wsWRK    = "Wrk"
wsSIM    = "Sim wrk"
wsTHESIS = "PhD wrk"
wsEXP    = "Exp wrk"
wsWRK4   = "wrk4"

myWorkspaces :: [[Char]]
myWorkspaces = [wsTMP, wsTMP2, wsPRO1, wsPRO2, wsPRO3, wsCON, wsPER, wsWRK, wsSIM, wsEXP, wsTHESIS, wsWRK4]

myWorkspaceIndices :: M.Map [Char] [Char]
myWorkspaceIndices = M.fromList $ zip myWorkspaces ["Home", "End", "1", "2", "3", "4", "5", "6", "7", "8", "9", "0"]

clickable :: [Char] -> [Char]
clickable ws = "<action=`xdotool key super+"++show i++"`>"++ws++"</action>"
    where i = fromJust $ M.lookup ws myWorkspaceIndices

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
                , projectStartHook  = Just $ do spawnOn wsPRO1 "prusaslicer"
                                                spawnOn wsPRO1 ("sleep 2; " ++ myBrowser)
                }
    , Project   { projectName       = wsPRO2
                , projectDirectory  = "~/Projects/ProjectLogs"
                , projectStartHook  = Just $ do spawnOn wsPRO2 (myTerminal ++ " --session=/home/oleete/.config/kitty/logs.conf")
                                                spawnOn wsPRO2 ("sleep .2; " ++ myBrowser)
                }
    , Project   { projectName       = wsPRO3
                , projectDirectory  = "~/Projects/D&D Home"
                , projectStartHook  = Just $ do spawnOn wsPRO3 (myLongBrowser ++ " --new-window 'https://roll20.net/welcome'")
                }
    , Project   { projectName       = wsCON
                , projectDirectory  = "~/.config"
                , projectStartHook  = Just $ do spawnOn wsCON (myTerminal ++ " --session=/home/oleete/.config/kitty/config.conf")
                                                spawnOn wsCON ("sleep .5; " ++ myBrowser)
                }
    , Project   { projectName       = wsPER
                , projectDirectory  = "~/PersonalDrive"
                , projectStartHook  = Just $ do spawnOn wsPER (myLongBrowser ++ " --new-window 'github.com' 'feedly.com/i/latest' 'youtube.com/feed/subscriptions' 'nebula.app/myshows' 'coinbase.com'")
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
                , projectDirectory  = "~/Projects/Thesis/0.1_LaTeX"
                , projectStartHook  = Just $ do spawnOn wsTHESIS (myTerminal ++ " --session=/home/oleete/.config/kitty/thesis.conf")
                                                spawnOn wsTHESIS ("sleep .2; " ++ myBrowser)
                                                spawnOn wsTHESIS "sleep .5; zathura /home/oleete/Projects/Thesis/0.1_LaTeX/OML-Thesis.pdf"
                }
    , Project   { projectName       = wsWRK4
                , projectDirectory  = "~/UniDrive"
                , projectStartHook  = Just $ do spawnOn wsWRK4 "sleep .3; browser"
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

discordCommand         = "discord"
gTasksCommand          = myBrowser ++ " '-tasks --app=chrome-extension://ndbaejgcaecffnhlmdghchfehkflgfkj/index.html --class=Tasks'"
gTasksWrkCommand       = myBrowser ++ " 'tasks --app=chrome-extension://ndbaejgcaecffnhlmdghchfehkflgfkj/index.html --class=WrkTasks'"
keepCommand            = myBrowser ++ " '-keep --app=https://keep.google.com/#home --class=Keep'"
keepWrkCommand         = myBrowser ++ " 'keep --app=https://keep.google.com/#home --class=WrkKeep'"
youtubeMusicCommand    = "$HOME/.local/bin/YouTube-Music-Desktop-App-1.13.0.AppImage"

scratchpads :: [NamedScratchpad]
scratchpads =
    [   NS "tasks" gTasksCommand (className =? "Tasks") nonFloating
    ,   NS "tasksWork"  gTasksWrkCommand (className =? "WrkTasks") nonFloating
    ,   NS "keepNsp" keepCommand (className =? "Keep") nonFloating
    ,   NS "keepWrkNsp"  keepWrkCommand (className =? "WrkKeep") nonFloating

    ,   NS "discord"  discordCommand (className =? "discord") defaultFloating
    ,   NS "youtubeMusic"  youtubeMusicCommand (className =? "youtube-music-desktop-app") nonFloating
    ,   NS "calc"  "gnome-calculator --class=calcu" (className =? "calcu") nonFloating
    ,   NS "console"  "kitty -1 --class=kittyconsole" (className =? "kittyconsole") nonFloating
    ]

----------------------------------------------------------------------------------------------------
-- Theme                                                                                     --
----------------------------------------------------------------------------------------------------
background = "#1a1b26"
foreground = "#a9b1d6"
dull       = "#565f89"
active     = "#7595E0"
visible    = "#9ece6a"
alert      = "#f7768e"

-- sizes
gap    = 4
reSize = 1/20
tabsh    = 20
myBorder = 3
prompt   = 30

myFont      = "xft:Ubuntu:weight=normal:pixelsize=16:antialias=true:hinting=true"
myWideFont  = "xft:Eurostar Black Extended:"
            ++ "style=Regular:pixelsize=180:hinting=true"

myTabTheme :: Theme
myTabTheme = def
    { fontName              = myFont
    , activeColor           = active
    , activeBorderColor     = active
    , activeTextColor       = background
    , inactiveColor         = background
    , inactiveBorderColor   = background
    , inactiveTextColor     = foreground
    , decoWidth             = 150
    , decoHeight            = tabsh
    }

myPromptTheme :: XPConfig
myPromptTheme = def
    { font                  = myFont
    , bgColor               = background
    , fgColor               = foreground
    , fgHLight              = visible
    , bgHLight              = background
    , borderColor           = active
    , promptBorderWidth     = myBorder
    , height                = prompt
    , position              = CenteredAt (1 / 4) (1 / 4)
    , searchPredicate       = fuzzyMatch
    , sorter                = fuzzySort
    , autoComplete          = Nothing
    }

hotPromptTheme :: XPConfig
hotPromptTheme = myPromptTheme
    { bgColor               = background
    , fgColor               = alert
    }

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
mySpacing = spacingRaw False (Border gap gap gap gap) True (Border gap gap gap gap) True

data FULLBAR = FULLBAR deriving (Read, Show, Eq, Typeable)
instance Transformer FULLBAR Window where
    transform FULLBAR x k = k barFull (const x)

barFull = renamed [Replace "Tabs"] $ avoidStruts $ addTabs shrinkText myTabTheme $ mySpacing
        $ SimpleFocus 1 (reSize/2) 0

data FULLCENTER = FULLCENTER deriving (Read, Show, Eq, Typeable)
instance Transformer FULLCENTER Window where
    transform FULLCENTER x k = k centerFull (const x)

centerFull = renamed [Replace "Tabs"] $ avoidStruts $ addTabs shrinkText myTabTheme $ mySpacing
           $ onWorkspaces [wsTHESIS] (SimpleFocus (1/4) (reSize/2) 1000)
           $ SimpleFocus (1/2) (reSize/2) 1500

myLayoutHook= smartBorders
            $ mkToggle (single FULL)
            $ mkToggle (single FULLBAR)
            $ mkToggle (single FULLCENTER)
            $ renamed [Replace "Notebook"]
            $ showWName' myShowWNameTheme
            $ addTabs shrinkText myTabTheme
            $ avoidStruts
            $ mySpacing
              notebookLayout
    where
    notebookMulti   = Notebook 2560  True True True 1 3 reSize 2 (2/3)
    notebookColumns = Notebook 1920 False True True 3 3 reSize 2 (2/3)
    notebookLayout = onWorkspaces [wsTMP, wsTMP2, wsPER, wsWRK] notebookColumns notebookMulti

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
easymotionConfig = def
    { overlayF = proportional (0.3 :: Double)
    , bgCol = background
    , txtCol = foreground
    , borderCol = active
    , cancelKey = xK_F8
    , sKeys = AnyKeys [xK_t, xK_n, xK_s, xK_e, xK_r, xK_i, xK_a, xK_o, xK_d, xK_h, xK_g, xK_j, xK_p
                      , xK_l, xK_f, xK_u, xK_w, xK_y, xK_b, xK_k, xK_v, xK_m, xK_c, xK_x, xK_z, xK_q]
    }

myModMask = mod4Mask
wsKeys = ["S-#", "#", "1", "2", "3", "4", "5", "6", "7", "8", "9", "0"]
zipM  m ks as f = zipWith (\k d -> (m ++ k, f d)) ks as

myKeys :: [(String, X ())]
myKeys =
    [ ("M-q"             , spawn "xmonad --restart")
    , ("M-C-q"           , spawn "xmonad --recompile && xmonad --restart")
    , ("M-S-q"           , confirmPrompt hotPromptTheme "Quit XMonad" $ io exitSuccess)
    , ("M-M1-C-S-x"      , spawn "slock")

    , ("M-M1-C-S-z"      , spawn "colorpicker")
    , ("M-M1-C-S-o"      , spawn "killall screenkey || screenkey")
    , ("M-M1-C-S-/"      , spawn "screenkey --show-settings")
    , ("M-M1-C-S-f"      , spawn "screencapt" )
    , ("M-M1-C-S-s"      , spawn "screencapt area" )
    , ("M-M1-C-S-w"      , spawn "screencast" )
    , ("M-M1-C-S-r"      , spawn "screencast area" )
    , ("M-M1-C-S-<Space>", spawn "playerctl play-pause" )
    , ("M-M1-C-S-<Left>" , spawn "playerctl previous" )
    , ("M-M1-C-S-<Right>", spawn "playerctl next" )

    , ("M-a"             , spawn "rofi -matching fuzzy -modi combi -show combi -combi-modi window,drun,run -show-icons")
    , ("M-<Return>"      , multiBind (P.sendKey (controlMask .|. shiftMask) xK_Return) (runOrRaise myTerminal (className =? "kitty")) (runOrRaise myTerminal (className =? "kitty")))
    , ("M-C-<Return>"    , spawn myTerminal)
    , ("M-b"             , multiBind (runOrRaise myBrowser (className =? "Google-chrome")) (P.sendKey controlMask xK_t) (runOrRaise myBrowser (className =? "Google-chrome")))
    , ("M-C-b"           , spawn myBrowser)
    , ("M-M1-b"          , altBrowser)
    , ("M-v"             , runOrRaise "zathura" (className =? "Zathura"))

    , ("M-S-c"           , allNamedScratchpadAction scratchpads "calc")
    , ("M-S-<Return>"    , allNamedScratchpadAction scratchpads "console")
    , ("M-S-d"           , allNamedScratchpadAction scratchpads "discord")
    , ("M-S-m"           , allNamedScratchpadAction scratchpads "youtubeMusic")
    , ("M-S-t"           , wrkNSP "tasksWork" "tasks")
    , ("M-S-n"           , wrkNSP "keepWrkNsp" "keepNsp")

    , ("M-<Backspace>"   , multiBind (spawn (myTerminalRemote ++ " delWindow")) (P.sendKey controlMask xK_w) kill)
    , ("M-C-<Backspace>" , kill)
    , ("M-S-<Backspace>" , confirmPrompt hotPromptTheme "kill all" killAll)

    , ("M-<Tab>"         , multiBind (spawn (myTerminalRemote ++ " tabSwap Right Tab")) (P.sendKey controlMask xK_Tab) (bindOn LD [("Tabs", windows W.focusDown)]))
    , ("M-S-<Tab>"       , multiBind (spawn (myTerminalRemote ++ " tabSwap Left shift+Tab")) (P.sendKey (controlMask .|. shiftMask) xK_Tab) (bindOn LD [("Tabs", windows W.focusUp)]))
    , ("M-C-<Tab>"       , bindOn LD [("Tabs", windows W.focusDown)])
    , ("M-C-S-<Tab>"     , bindOn LD [("Tabs", windows W.focusUp)])
    , ("M-t"             , multiBind (P.sendKey (controlMask .|. shiftMask) xK_t) (P.sendKey controlMask xK_t) (spawn ""))

    , ("M-f"             , multiBind (P.sendKey (controlMask .|. shiftMask) xK_f) (P.sendKey noModMask xK_F11) (toggleLayout FULL))
    , ("M-C-f"           , toggleLayout FULL)
    , ("M-s"             , toggleLayout FULLBAR)
    , ("M-c"             , toggleLayout FULLCENTER)

    , ("<F8>"            , selectWindow easymotionConfig >>= (`whenJust` windows . W.focusWindow))
    , ("M-h"             , kittyBind " moveWindow left h"   (windowGo L True))
    , ("M-j"             , kittyBind " moveWindow bottom j" (windowGo D True))
    , ("M-k"             , kittyBind " moveWindow top k"    (windowGo U True))
    , ("M-l"             , kittyBind " moveWindow right l"  (windowGo R True))
    , ("M-C-S-h"         , windowGo L True)
    , ("M-C-S-j"         , windowGo D True)
    , ("M-C-S-k"         , windowGo U True)
    , ("M-C-S-l"         , windowGo R True)
    , ("M-C-h"           , windowSwap L True)
    , ("M-C-j"           , windowSwap D True)
    , ("M-C-k"           , windowSwap U True)
    , ("M-C-l"           , windowSwap R True)

    , ("M-m"             , kittyBind " mainMove" (swapPromote' False))
    , ("M-C-m"           , swapPromote' False)
    , ("M-y"             , withFocused toggleFloat)
    , ("M-C-y"           , sinkAll)

    , ("M-,"             , sendMessage (IncMasterN (-1)))
    , ("M-."             , sendMessage (IncMasterN 1))
    , ("M-C-,"           , sendMessage (IncColumnN (-1)))
    , ("M-C-."           , sendMessage (IncColumnN 1))
    , ("M-["             , sendMessage Shrink)
    , ("M-]"             , sendMessage Expand)
    , ("M-C-["           , sendMessage MirrorShrink)
    , ("M-C-]"           , sendMessage MirrorExpand)

    , ("M-r"             , sendMessage ToggleSide)
    , ("M-C-r"           , sendMessage ToggleStackDir)
    , ("M-S-x"           , sendMessage ToggleMiddle)

    , ("M-w"             , switchProjectPrompt myPromptTheme)
    , ("M-C-w"           , shiftToProjectPrompt myPromptTheme)
    , ("M-<Space>"       , toggleWS' ["NSP"])
    , ("M-C-<Space>"     , shiftToggleWS' ["NSP"])
    ]
    ++ zipM "M-"         wsKeys [0..] (withNthWorkspace W.greedyView)
    ++ zipM "M-C-"       wsKeys [0..] (withNthWorkspace W.shift)
    where
        toggleFloat w = windows (\s -> if M.member w (W.floating s)
                            then W.sink w s
                            else W.float w (W.RationalRect (1/4) (1/4) (1/2) (1/2)) s)

        altBrowser = bindOn WS [(wsTMP2,   spawn myLongBrowser) ,(wsTMP,   spawn myLongBrowser)
                               ,(wsWRK,   spawn myLongBrowser) ,(wsWRK4,  spawn myLongBrowser)
                               ,(wsTHESIS,spawn myLongBrowser) ,(wsEXP,   spawn myLongBrowser)
                               ,(wsSIM,   spawn myLongBrowser) ,("",      spawn (myLongBrowser ++ "-wrk"))]

        wrkNSP work personal = bindOn WS [(wsWRK, allNamedScratchpadAction scratchpads work)
                                         ,(wsSIM, allNamedScratchpadAction scratchpads work)
                                         ,(wsEXP, allNamedScratchpadAction scratchpads work)
                                         ,(wsTHESIS, allNamedScratchpadAction scratchpads work)
                                         ,(wsWRK4, allNamedScratchpadAction scratchpads work)
                                         ,("", allNamedScratchpadAction scratchpads personal)]

        kittyBind kitty leftover = bindFirst [(className =? "kitty", spawn (myTerminalRemote ++ kitty))
                                          ,(className =? "kittyconsole", spawn (myTerminalRemote ++ kitty))
                                          ,(pure True, leftover)]

        multiBind kitty chrome leftover = bindFirst [(className =? "kitty", kitty)
                                                    ,(className =? "kittyconsole", kitty)
                                                    ,(className =? "Google-chrome", chrome)
                                                    ,(pure True, leftover)]
        toggleLayout layout = sequence_ [ withFocused $ windows . W.sink, sendMessage $ XMonad.Layout.MultiToggle.Toggle layout ]

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
    spawn "feh --bg-fill --randomize ~/Pictures/wallpapers/"
    spawnOnce "picom -b --config ~/.config/picom/picom.conf"
    spawnOnce "insync start; insync hide"
    spawnOnce "xsetroot -cursor_name left_ptr"
    spawnOnce "deadd-notification-center &"

----------------------------------------------------------------------------------------------------
-- Log                                                                                            --
----------------------------------------------------------------------------------------------------

myLogHook :: Handle -> X ()
myLogHook h = do
    fadeWindowsLogHook myFadeHook
    ewmhDesktopsLogHook
    masterHistoryHook
    dynamicLogWithPP . filterOutWsPP ["NSP"] $ def
        { ppCurrent             = xmobarColor active "" . wrap "[" "]" . clickable
        , ppTitle               = const ""
        , ppVisible             = xmobarColor visible  "" . clickable
        , ppHidden              = xmobarColor dull  "" . clickable
        , ppHiddenNoWindows     = const ""
        , ppSep                 = " | "
        , ppWsSep               = " | "
        , ppLayout              = const ""
        , ppOrder               = id
        , ppOutput              = hPutStrLn h
        , ppSort                = ppSort def }

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
            , resource =? "stalonetray"    -?> doIgnore

            , resource =? "gnome-calculator" -?> doCenterFloat
            , resource =? "pavucontrol" -?> doRectFloat (W.RationalRect (1285/3840) (31/2160) (600/3840) (800/2160))

            , resource =? "Tasks" -?> doRectFloat halfNhalf
            , resource =? "WrkTasks" -?> doRectFloat halfNhalf
            , className =? "Keep" -?> doRectFloat halfNhalf
            , className =? "WrkKeep" -?> doRectFloat halfNhalf
            , resource =? "kittyconsole" -?> doRectFloat (W.RationalRect (3 / 5) (3 / 5) (1 / 3) (1 / 3))
            , resource =? "youtube-music-desktop-app" -?> doRectFloat halfNhalf
            , resource =? "discord" -?> doRectFloat halfNhalf

            , transience
            , isBrowserDialog -?> forceCenterFloat
            , isRole =? "GtkFileChooserDialog" -?> forceCenterFloat
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
myHandleEventHook = docksEventHook
                <+> fadeWindowsEventHook
                <+> handleEventHook def
                <+> XMonad.Util.Hacks.windowedFullscreenFixEventHook

----------------------------------------------------------------------------------------------------
-- Custom hook helpers                                                                            --
----------------------------------------------------------------------------------------------------

forceCenterFloat :: ManageHook
forceCenterFloat = doFloatDep move
  where
    move :: W.RationalRect -> W.RationalRect
    move _ = W.RationalRect x y w h

    w, h, x, y :: Rational
    w = 1/3
    h = 1/2
    x = (1-w)/2
    y = (1-h)/2
