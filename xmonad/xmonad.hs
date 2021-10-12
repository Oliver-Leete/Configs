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
import XMonad.Util.NamedActions
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
        $ addDescrKeys' ((controlMask .|. shiftMask .|. mod4Mask .|. mod1Mask, xK_semicolon), showKeybindings) myKeys
        $ myConfig xmproc0

myConfig p = def
        { borderWidth        = myBorder
        , clickJustFocuses   = myClickJustFocuses
        , focusFollowsMouse  = myFocusFollowsMouse
        , normalBorderColor  = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , manageHook         = myManageHook
        , handleEventHook    = myHandleEventHook
        , layoutHook         = myLayoutHook
        , logHook            = myLogHook p
        , modMask            = myModMask
        , mouseBindings      = myMouseBindings
        , startupHook        = myStartupHook
        , terminal           = myTerminal
        , workspaces         = myWorkspaces
        }


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
myWorkspaceIndices = M.fromList $ zip myWorkspaces ["Home", "End", "1", "2", "3", "4", "5", "6", "7", "8", "9", "0"] -- (,) == \x y -> (x,y)

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
                , projectStartHook  = Just $    spawnOn wsPRO3 "google-chrome-stable --user-data-dir=/home/oleete/.config/browser/google-chrome-stable --new-window 'https://roll20.net/welcome'"
                }

    , Project   { projectName       = wsCON
                , projectDirectory  = "~/.config"
                , projectStartHook  = Just $ do spawnOn wsCON (myTerminal ++ " --session=/home/oleete/.config/kitty/config.conf")
                                                spawnOn wsCON ("sleep .5; " ++ myBrowser)
                }

    , Project   { projectName       = wsPER
                , projectDirectory  = "~/PersonalDrive"
                , projectStartHook  = Just $    spawnOn wsPER "google-chrome-stable --user-data-dir=/home/oleete/.config/browser/google-chrome-stable --new-window 'github.com' 'feedly.com/i/latest' 'youtube.com/feed/subscriptions' 'nebula.app/myshows' 'coinbase.com'"
                }

    , Project   { projectName       = wsWRK
                , projectDirectory  = "~/UniDrive"
                , projectStartHook  = Just $    spawnOn wsWRK "google-chrome-stable --user-data-dir=/home/oleete/.config/browser/google-chrome-stable-wrk --new-window 'sheffield.ac.uk' 'gmail.com' 'calendar.google.com' 'chrome-extension://ndbaejgcaecffnhlmdghchfehkflgfkj/index.html' 'keep.google.com' 'drive.google.com' 'github.com/Oliver-Leete/ThesisLatex' 'feedly.com/i/latest' 'paperpile.com/app'"
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
                , projectStartHook  = Just $    spawnOn wsWRK4 "sleep .3; browser"
                }
    ]

----------------------------------------------------------------------------------------------------
-- Applications                                                                                   --
----------------------------------------------------------------------------------------------------

myTerminal     = "kitty --single-instance --listen-on unix:/tmp/mykitty"
myTerminalRemote = "kitty @ --to unix:/tmp/mykitty launch --type=background"
myBrowser      = "/home/oleete/.config/bin/browser"
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

myFocusFollowsMouse  = True
myClickJustFocuses   = True

background = "#1a1b26"
foreground = "#a9b1d6"
dull       = "#565f89"
active     = "#7595E0"
visible    = "#9ece6a"
alert      = "#f7768e"

-- sizes
gap    = 4
reSize = 1/20

-- topbar   = 8
tabsh    = 20
myBorder = 3
prompt   = 30

myNormalBorderColor     = background
myFocusedBorderColor    = active

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
easymotionConfig = def
    { overlayF = proportional (0.3 :: Double)
      , bgCol = background
      , txtCol = foreground
      , borderCol = active
      , cancelKey = xK_F8
      , sKeys = AnyKeys [xK_t, xK_n, xK_s, xK_e, xK_r, xK_i, xK_a, xK_o, xK_d, xK_h, xK_g, xK_j
                , xK_p, xK_l, xK_f, xK_u, xK_w, xK_y, xK_b, xK_k, xK_v, xK_m, xK_c, xK_x, xK_z, xK_q]
    }

----------------------------------------------------------------------------------------------------
-- Layouts                                                                                        --
----------------------------------------------------------------------------------------------------

myNav2DConf :: Navigation2DConfig
myNav2DConf = def
    { defaultTiledNavigation    = centerNavigation
    , floatNavigation           = centerNavigation
    , screenNavigation          = lineNavigation
    , layoutNavigation          = [("Full", centerNavigation)]
    , unmappedWindowRect        = [("Full", singleWindowRect)]
    }

mySpacing = spacingRaw False (Border gap gap gap gap) True (Border gap gap gap gap) True

data FULLBAR = FULLBAR deriving (Read, Show, Eq, Typeable)
instance Transformer FULLBAR Window where
    transform FULLBAR x k = k barFull (const x)

barFull = renamed [Replace "Tabs"] $ avoidStruts $ addTabs shrinkText myTabTheme $ mySpacing $ SimpleFocus 1 (reSize/2) 0

data FULLCENTER = FULLCENTER deriving (Read, Show, Eq, Typeable)
instance Transformer FULLCENTER Window where
    transform FULLCENTER x k = k centerFull (const x)

centerFull = renamed [Replace "Tabs"] $ avoidStruts $ addTabs shrinkText myTabTheme $ mySpacing
           $ onWorkspaces [wsTHESIS] (SimpleFocus (1/4) (reSize/2) 1000)
           $ SimpleFocus (1/2) (reSize/2) 1500

myLayoutHook= smartBorders
            $ fullScreenToggle $ fullBarToggle $ fullCenterToggle
            $ showWorkspaceName
            $ addTabs shrinkText myTabTheme
            $ avoidStruts
            $ mySpacing
              notebookLayout
    where
    notebookMulti   = Notebook 2560  True True True 1 3 reSize 2 (2/3)
    notebookColumns = Notebook 1920 False True True 3 3 reSize 2 (2/3)
    notebookLayout = onWorkspaces [wsTMP, wsTMP2, wsPER, wsWRK] notebookColumns notebookMulti

    -- Other Layout Stuff
    fullBarToggle       = mkToggle (single FULLBAR)
    fullCenterToggle    = mkToggle (single FULLCENTER)
    fullScreenToggle    = mkToggle (single FULL)
    showWorkspaceName   = showWName' myShowWNameTheme

----------------------------------------------------------------------------------------------------
-- Keybindings                                                                                    --
----------------------------------------------------------------------------------------------------

myModMask :: KeyMask
myModMask = mod4Mask

showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings x = addName "Show Keybindings" $ io $ do
    h <- spawnPipe "zenity --text-info --font=terminus"
    hPutStr h (unlines $ showKm x)
    hClose h
    return ()

myKeys :: XConfig Layout -> [((KeyMask, KeySym), NamedAction)]
myKeys conf = let

    subKeys str ks = subtitle str : mkNamedKeymap conf ks
    wsKeys = ["S-#", "#", "1", "2", "3", "4", "5", "6", "7", "8", "9", "0"]

    zipM  m nm ks as f = zipWith (\k d -> (m ++ k, addName nm $ f d)) ks as

    toggleFloat w = windows (\s -> if M.member w (W.floating s)
                    then W.sink w s
                    else W.float w (W.RationalRect (1/4) (1/4) (1/2) (1/2)) s)

    in

    subKeys "System"
    [ ("M-q"             , addName "Restart XMonad"              $ spawn "xmonad --restart")
    , ("M-C-q"           , addName "Rebuild & restart XMonad"    $ spawn "xmonad --recompile && xmonad --restart")
    , ("M-S-q"           , addName "Quit XMonad"                 $ confirmPrompt hotPromptTheme "Quit XMonad" $ io exitSuccess)
    , ("M-M1-C-S-x"      , addName "Lock screen"                 $ spawn "slock")
    ] ^++^

    subKeys "Utilities"
    [ ("M-M1-C-S-z"      , addName "Colour picker"               $ spawn "colorpicker")
    , ("M-M1-C-S-o"      , addName "On-screen keys"              $ spawn "killall screenkey || screenkey")
    , ("M-M1-C-S-/"      , addName "On-screen keys settings"     $ spawn "screenkey --show-settings")

    , ("M-M1-C-S-f"      , addName "Capture screen"              $ spawn "screencapt" )
    , ("M-M1-C-S-s"      , addName "Capture selection"           $ spawn "screencapt area" )
    , ("M-M1-C-S-w"      , addName "Record screen"               $ spawn "screencast" )
    , ("M-M1-C-S-r"      , addName "Record area"                 $ spawn "screencast area" )

    , ("M-M1-C-S-<Space>", addName "Play/Pause"                  $ spawn "playerctl play-pause" )
    , ("M-M1-C-S-<Left>" , addName "Skip Song"                   $ spawn "playerctl previous" )
    , ("M-M1-C-S-<Right>", addName "Prev Song"                   $ spawn "playerctl next" )

    , ("M-u"             , addName "Scroll Up"                   $ spawn "xdotool click 4; sleep 0.001; xdotool click 4; sleep 0.001; xdotool click 4; sleep 0.001; xdotool click 4; sleep 0.001; xdotool click 4; sleep 0.001; xdotool click 4")
    , ("M-d"             , addName "Scroll Down"                 $ spawn "xdotool click 5; sleep 0.001; xdotool click 5; sleep 0.001; xdotool click 5; sleep 0.001; xdotool click 5; sleep 0.001; xdotool click 5; sleep 0.001; xdotool click 5")
    ] ^++^

    subKeys "Apps"
    [ ("M-<Return>"      , addName "Terminal"                    $ bindFirst [(className =? "kitty", P.sendKey (controlMask .|. shiftMask) xK_Return)
                                                                             ,(className =? "kittyconsole", P.sendKey (controlMask .|. shiftMask) xK_Return)
                                                                             ,(pure True, runOrRaise myTerminal (className =? "kitty"))])
    , ("M-C-<Return>"    , addName "Force Terminal"              $ spawn myTerminal)
    , ("M-b"             , addName "Browser"                     $ bindFirst [(className =? "Google-chrome", P.sendKey controlMask xK_t)
                                                                             ,(pure True, runOrRaise myBrowser (className =? "Google-chrome"))])
    , ("M-v"             , addName "PDF Viewer"                  $ runOrRaise "zathura" (className =? "Zathura"))
    , ("M-C-b"           , addName "Force Browser"               $ spawn myBrowser)
    , ("M-M1-b"          , addName "Alternate Browser"           $ bindOn WS [(wsTMP2,   spawn "google-chrome-stable --user-data-dir='/home/oleete/.config/browser/google-chrome-stable'")
                                                                             ,(wsTMP,    spawn "google-chrome-stable --user-data-dir='/home/oleete/.config/browser/google-chrome-stable'")
                                                                             ,(wsWRK,    spawn "google-chrome-stable --user-data-dir='/home/oleete/.config/browser/google-chrome-stable'")
                                                                             ,(wsWRK4,   spawn "google-chrome-stable --user-data-dir='/home/oleete/.config/browser/google-chrome-stable'")
                                                                             ,(wsTHESIS, spawn "google-chrome-stable --user-data-dir='/home/oleete/.config/browser/google-chrome-stable'")
                                                                             ,(wsEXP,    spawn "google-chrome-stable --user-data-dir='/home/oleete/.config/browser/google-chrome-stable'")
                                                                             ,(wsSIM,    spawn "google-chrome-stable --user-data-dir='/home/oleete/.config/browser/google-chrome-stable'")
                                                                             ,("",       spawn "google-chrome-stable --user-data-dir='/home/oleete/.config/browser/google-chrome-stable-wrk'")])
    ] ^++^

    subKeys "ScratchPad Apps"
    [ ("M-S-c"           , addName "NSP Calculator"              $ allNamedScratchpadAction scratchpads "calc")
    , ("M-S-<Return>"    , addName "NSP Console"                 $ allNamedScratchpadAction scratchpads "console")
    , ("M-S-d"           , addName "NSP Discord"                 $ allNamedScratchpadAction scratchpads "discord")
    , ("M-S-m"           , addName "NSP Music"                   $ allNamedScratchpadAction scratchpads "youtubeMusic")
    , ("M-S-t"           , addName "NSP Tasks"                   $ bindOn WS [(wsWRK, allNamedScratchpadAction scratchpads "tasksWork")
                                                                             ,(wsSIM, allNamedScratchpadAction scratchpads "tasksWork")
                                                                             ,(wsEXP, allNamedScratchpadAction scratchpads "tasksWork")
                                                                             ,(wsTHESIS, allNamedScratchpadAction scratchpads "tasksWork")
                                                                             ,(wsWRK4, allNamedScratchpadAction scratchpads "tasksWork")
                                                                             ,("", allNamedScratchpadAction scratchpads "tasks")])
    , ("M-S-n"           , addName "NSP Keep"                    $ bindOn WS [(wsWRK, allNamedScratchpadAction scratchpads "keepWrkNsp")
                                                                             ,(wsSIM, allNamedScratchpadAction scratchpads "keepWrkNsp")
                                                                             ,(wsEXP, allNamedScratchpadAction scratchpads "keepWrkNsp")
                                                                             ,(wsTHESIS, allNamedScratchpadAction scratchpads "keepWrkNsp")
                                                                             ,(wsWRK4, allNamedScratchpadAction scratchpads "keepWrkNsp")
                                                                             ,("", allNamedScratchpadAction scratchpads "keepNsp")])
    ] ^++^

    subKeys "Workspaces and Projects"
    (
    [ ("M-a"             , addName "Launcher"                    $ spawn "rofi -matching fuzzy -modi combi -show combi -combi-modi window,drun,run -show-icons")
    , ("M-w"             , addName "prompt select ws"            $ switchProjectPrompt myPromptTheme)
    , ("M-C-w"           , addName "prompt send to ws"           $ shiftToProjectPrompt myPromptTheme)
    , ("M-<Space>"       , addName "Swap with last workspace"    $ toggleWS' ["NSP"])
    , ("M-C-<Space>"     , addName "Send to last workspace"      $ shiftToggleWS' ["NSP"])
    ]
    ++ zipM "M-"         "View ws"                               wsKeys [0..] (withNthWorkspace W.greedyView)
    ++ zipM "M-C-"       "Move w to ws"                          wsKeys [0..] (withNthWorkspace W.shift)
    ) ^++^

    subKeys "Windows"
    [ ("<F8>"            , addName "Hop to Window"               $ selectWindow easymotionConfig >>= (`whenJust` windows . W.focusWindow))

    , ("M-<Backspace>"   , addName "Kill"                        $ bindFirst [(className =? "kitty", spawn (myTerminalRemote ++ " delWindow"))
                                                                             ,(className =? "kittyconsole",  spawn (myTerminalRemote ++ " delWindow"))
                                                                             ,(className =? "Google-chrome", P.sendKey controlMask xK_w)
                                                                             ,(pure True, kill)])
    , ("M-C-<Backspace>" , addName "Force kill"                  kill)
    , ("M-S-<Backspace>" , addName "Kill all"                    $ confirmPrompt hotPromptTheme "kill all" killAll)

    , ("M-<Tab>"         , addName "Next Tab"                    $ bindFirst [(className =? "kitty", spawn (myTerminalRemote ++ " tabSwap Right Tab"))
                                                                             ,(className =? "kittyconsole", spawn (myTerminalRemote ++ " tabSwap Right Tab"))
                                                                             ,(className =? "Google-chrome", P.sendKey controlMask xK_Tab)
                                                                             ,(pure True, bindOn LD [("Tabs", windows W.focusDown)])])
    , ("M-S-<Tab>"       , addName "Previous Tab"                $ bindFirst [(className =? "kitty", spawn (myTerminalRemote ++ " tabSwap Left shift+Tab"))
                                                                             ,(className =? "kittyconsole", spawn (myTerminalRemote ++ " tabSwap Left shift+Tab"))
                                                                             ,(className =? "Google-chrome", P.sendKey (controlMask .|. shiftMask) xK_Tab)
                                                                             ,(pure True, bindOn LD [("Tabs", windows W.focusUp)])])
    , ("M-C-<Tab>"       , addName "Force Next Tab"              $  bindOn LD [("Tabs", windows W.focusDown)])
    , ("M-C-S-<Tab>"     , addName "Force Previous Tab"          $  bindOn LD [("Tabs", windows W.focusUp)])

    , ("M-t"             , addName "New Tab"                     $ bindFirst [(className =? "kitty", P.sendKey (controlMask .|. shiftMask) xK_t)
                                                                             ,(className =? "kittyconsole", P.sendKey (controlMask .|. shiftMask) xK_t)
                                                                             ,(className =? "Google-chrome", P.sendKey controlMask xK_t)])


    , ("M-f"             , addName "Fullscreen"                  $ bindFirst [(className =? "kitty", P.sendKey (controlMask .|. shiftMask) xK_f)
                                                                             ,(className =? "kittyconsole", P.sendKey (controlMask .|. shiftMask) xK_f)
                                                                             ,(pure True, sequence_ [ withFocused $ windows . W.sink, sendMessage $ XMonad.Layout.MultiToggle.Toggle FULL ])])
    , ("M-C-f"           , addName "Fullscreen"                  $ sequence_ [ withFocused $ windows . W.sink, sendMessage $ XMonad.Layout.MultiToggle.Toggle FULL ])
    , ("M-s"             , addName "Maximize"                    $ sequence_ [ withFocused $ windows . W.sink, sendMessage $ XMonad.Layout.MultiToggle.Toggle FULLBAR ])
    , ("M-c"             , addName "Center Focus"                $ sequence_ [ withFocused $ windows . W.sink, sendMessage $ XMonad.Layout.MultiToggle.Toggle FULLCENTER])
    ]^++^

    subKeys "Navigation"
    -- [ ("M-m"             , addName "Swap with main"             $ bindFirst [(className =? "kitty", P.sendKey (controlMask .|. shiftMask) xK_F9)
    [ ("M-m"             , addName "Swap with main"             $ bindFirst [(className =? "kitty", spawn (myTerminalRemote ++ " mainMove"))
                                                                            ,(className =? "kitty", spawn (myTerminalRemote ++ " mainMove"))
                                                                            ,(pure True, swapPromote' False)])
    , ("M-C-m"           , addName "Force Swap with main"       $ swapPromote' False)

    , ("M-h"             , addName "Navigate Left"               $ bindFirst [(className =? "kitty", spawn (myTerminalRemote ++ " movingNshaking left h"))
                                                                                        ,(className =? "kittyconsole", spawn (myTerminalRemote ++ " movingNshaking left h"))
                                                                                        ,(pure True, windowGo L False)])
    , ("M-j"             , addName "Navigate Down"               $ bindFirst [(className =? "kitty", spawn (myTerminalRemote ++ " movingNshaking bottom j"))
                                                                                        ,(className =? "kittyconsole", spawn (myTerminalRemote ++ " movingNshaking bottom j"))
                                                                                        ,(pure True, windowGo D False)])
    , ("M-k"             , addName "Navigate Up"                 $ bindFirst [(className =? "kitty", spawn (myTerminalRemote ++ " movingNshaking top k"))
                                                                                        ,(className =? "kittyconsole", spawn (myTerminalRemote ++ " movingNshaking top k"))
                                                                                        ,(pure True, windowGo U False)])
    , ("M-l"             , addName "Navigate Right"              $ bindFirst [(className =? "kitty", spawn (myTerminalRemote ++ " movingNshaking right l"))
                                                                                        ,(className =? "kittyconsole", spawn (myTerminalRemote ++ " movingNshaking right l"))
                                                                                        ,(pure True, windowGo R False)])

    , ("M-C-S-h"         , addName "Force Navigate Left"         $ windowGo L False)
    , ("M-C-S-j"         , addName "Force Navigate Down"         $ windowGo D False)
    , ("M-C-S-k"         , addName "Force Navigate Up"           $ windowGo U False)
    , ("M-C-S-l"         , addName "Force Navigate Right"        $ windowGo R False)

    , ("M-C-h"           , addName "Swap Left"                   $ windowSwap L False)
    , ("M-C-j"           , addName "Swap Down"                   $ windowSwap D False)
    , ("M-C-k"           , addName "Swap Up"                     $ windowSwap U False)
    , ("M-C-l"           , addName "Swap Right"                  $ windowSwap R False)
    ] ^++^

    subKeys "Layout Management"
    [ ("M-y"             , addName "Toggle window floating"      $ withFocused toggleFloat)
    , ("M-C-y"           , addName "Tile all floating w"         sinkAll)

    , ("M-,"             , addName "Decrease main windows"       $ sendMessage (IncMasterN (-1)))
    , ("M-."             , addName "Increase main windows"       $ sendMessage (IncMasterN 1))
    , ("M-C-,"           , addName "Decrease big windows"        $ sendMessage (IncColumnN (-1)))
    , ("M-C-."           , addName "Increase big windows"        $ sendMessage (IncColumnN 1))

    , ("M-["             , addName "Shrink Main"                 $ sendMessage Shrink)
    , ("M-]"             , addName "Expand Main"                 $ sendMessage Expand)
    , ("M-C-["           , addName "Shrink height"               $ sendMessage MirrorShrink)
    , ("M-C-]"           , addName "Expand height"               $ sendMessage MirrorExpand)
    , ("M-M1-["          , addName "Fast Shrink Main"            $ sequence_ [sendMessage Shrink, sendMessage Shrink])
    , ("M-M1-]"          , addName "Fast Expand Main"            $ sequence_ [sendMessage Expand, sendMessage Expand])
    , ("M-M1-C-["        , addName "Fast Shrink height"          $ sequence_ [sendMessage MirrorShrink, sendMessage MirrorShrink])
    , ("M-M1-C-]"        , addName "Fast Expand height"          $ sequence_ [sendMessage MirrorExpand, sendMessage MirrorExpand])

    , ("M-r"             , addName "Reflect"                     $ sendMessage ToggleSide)
    , ("M-C-r"           , addName "Reflect Stack"               $ sendMessage ToggleStackDir)

    , ("M-x"             , addName "Toggle Layout"               $ sendMessage ToggleMiddle)
    , ("M-C-x"           , addName "Reset layout"                $ setLayout $ XMonad.layoutHook conf)
    ]

myMouseBindings :: XConfig l -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings XConfig {} = M.fromList
    [ ((myModMask,               button1) ,\w -> focus w
      >> mouseMoveWindow w
      >> windows W.shiftMaster)

    , ((myModMask .|. shiftMask, button1), \w -> focus w
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
    spawnOnce "picom -b --config ~/.config/picom/picom.conf"
    spawn "feh --bg-fill --randomize ~/Pictures/wallpapers/"
    -- spawnOnce myTray
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
        , ppSort                = ppSort def
        , ppExtras              = [] }


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

-- All comments below were here when I got here

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
