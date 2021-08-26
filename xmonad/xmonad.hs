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
-- https://github.com/oliver-leete                                                                 --
----------------------------------------------------------------------------------------------------
-- As of May 2021 this should be using github xmonad/xmonad-contrib This is mostly taken from     --
-- Ethan Schoonover's config and from those before him, I have moved a decent bit around but have --
-- tried to keep all credit to others in. I am taking credit for things I'll just say if it looks --
-- like good code, there's no way I wrote it                                                      --

----------------------------------------------------------------------------------------------------
-- Modules                                                                                        --
----------------------------------------------------------------------------------------------------
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import System.Exit
import System.IO
import Graphics.X11.Types

import XMonad hiding ( (|||) )
import qualified XMonad.StackSet as W

import XMonad.Actions.ConditionalKeys
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicProjects
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.EasyMotion
import XMonad.Actions.Navigation2D
import XMonad.Actions.PerWindowKeys
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves
import XMonad.Actions.SinkAll
import XMonad.Actions.SpawnOn
import XMonad.Actions.SwapPromote
import XMonad.Actions.Warp
import XMonad.Actions.WithAll
import XMonad.Actions.WindowGoLocal

import XMonad.Hooks.DynamicLog
-- import XMonad.Hooks.DynamicProperty
import XMonad.Hooks.EwmhDesktops ( ewmhDesktopsLogHook, ewmh )
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ToggleHook

import XMonad.Layout.BorderResize
-- import XMonad.Layout.Column
import XMonad.Layout.DraggingVisualizer
-- import XMonad.Layout.FourColumns
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Master
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.Notebook
-- import XMonad.Layout.OneBig
-- import XMonad.Layout.PerScreen
import XMonad.Layout.PerWorkspace
import XMonad.Layout.PositionStoreFloat
import XMonad.Layout.Reflect
import XMonad.Layout.Renamed
import XMonad.Layout.ShowWName
import XMonad.Layout.SimpleFocus
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.WindowSwitcherDecoration
import XMonad.Layout.WindowNavigation

import XMonad.Prompt
import XMonad.Prompt.ConfirmPrompt
import XMonad.Prompt.FuzzyMatch
-- import XMonad.Prompt.Window

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

-- The main thing.

main :: IO ()
main = do
    xmproc0 <- spawnPipe myStatusBar

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

-- I have my number keys in a layer under my home row, so these are set to have all of my personal
-- layouts under my left hand and my work under my right. The "admin groups" for work and personal
-- are in-between the two. The TMP 1&2 workspaces are hidden elsewhere mostly as place holders so no
-- applications are opened at start up, with the bonus of having nice wallpapers that I can bring up
-- again without closing programs (it's nice to set the second monitor to a pretty picture instead
-- of just being surrounded by applications)

wsTMP    = "Tmp"
wsTMP2   = "Tmp2"
wsPRO1   = "Print"
wsPRO2   = "Notes"
wsPRO3   = "Dnd"
wsCON    = "Configs"
wsPER    = "Home"
wsFLOAT  = "Flt"
wsWRK    = "Wrk"
wsSIM    = "Sim wrk"
wsTHESIS = "PhD wrk"
wsEXP    = "Exp wrk"
wsWRK4   = "wrk4"

myWorkspaces :: [[Char]]
myWorkspaces = [wsTMP, wsTMP2, wsPRO1, wsPRO2, wsPRO3, wsCON, wsPER, wsWRK, wsSIM, wsEXP, wsTHESIS, wsWRK4, wsFLOAT]

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
                , projectStartHook  = Just $    spawnOn wsPER "google-chrome-stable --user-data-dir=/home/oleete/.config/browser/google-chrome-stable --new-window 'github.com' 'feedly.com/i/latest' 'youtube.com/feed/subscriptions' 'coinbase.com'"
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

-- Just a general list of all the programs I reference elsewhere. I should be more consistent with
-- what I put here. Either I should only be putting things used regularly, or I should be putting
-- anything.

myTerminal     = "kitty"
myBrowser      = "browser"
myBrowserClass = "google-chrome-stable"
myStatusBar    = "/home/oleete/.config/xmobar/init-xmobars"
myTray         = "trayer --edge top --align center --distance 5 --heighttype pixel --height 18 --widthtype request --transparent true --alpha 0 --tint 0x1a1b26"
myLauncher     = "rofi -matching fuzzy -modi combi -show combi -combi-modi window,drun,run -show-icons"
myLockscreen   = "slock"
myExplorer     = "nemo"
myCompositor   = "picom -b --config ~/.config/picom/picom.conf"
myColorPicker  = "colorpicker"
-- Lets see if this fixes the below bug
myWallpaper    = "feh --bg-fill --randomize ~/Pictures/wallpapers/"

-- Named scratchpads, using browser script and bindOn to have some of them be workspace based so the
-- work tasks and keep are on the work layouts and the personal on personal.

discordCommand         = "discord"
discordResource        = "discord"
isDiscord              = className =? discordResource

gTasksCommand          = myBrowser ++ " '-tasks --app=chrome-extension://ndbaejgcaecffnhlmdghchfehkflgfkj/index.html --class=Tasks'"
gTasksResource         = "Tasks"
isTasks                = className =? gTasksResource

gTasksWrkCommand       = myBrowser ++ " 'tasks --app=chrome-extension://ndbaejgcaecffnhlmdghchfehkflgfkj/index.html --class=WrkTasks'"
gTasksWrkResource      = "WrkTasks"
isTasksWrk             = className =? gTasksWrkResource

keepCommand            = myBrowser ++ " '-keep --app=https://keep.google.com/#home --class=Keep'"
keepResource           = "Keep"
isKeep                 = className =? keepResource

keepWrkCommand         = myBrowser ++ " 'keep --app=https://keep.google.com/#home --class=WrkKeep'"
keepWrkResource        = "WrkKeep"
isKeepWrk              = className =? keepWrkResource

chromenspCommand       = myBrowser ++ " 'float --class=Chromensp'"
chromenspResource      = "Chromensp"
isChromensp            = className =? chromenspResource

chromenspWrkCommand    = myBrowser ++ " 'wrkfloat --class=Chromewrknsp'"
chromenspWrkResource   = "Chromewrknsp"
isChromenspWrk         = className =? chromenspWrkResource

youtubeMusicCommand    = "$HOME/.local/bin/YouTube-Music-Desktop-App-1.13.0.AppImage"
youtubeMusicResource   = "youtube-music-desktop-app"
isYoutubeMusic         = className =? youtubeMusicResource

scratchpads :: [NamedScratchpad]
scratchpads =
    [   NS "tasks" gTasksCommand isTasks nonFloating
    ,   NS "tasksWork"  gTasksWrkCommand isTasksWrk nonFloating

    ,   NS "keepNsp" keepCommand isKeep nonFloating
    ,   NS "keepWrkNsp"  keepWrkCommand isKeepWrk nonFloating

    ,   NS "chromensp"  chromenspCommand isChromensp nonFloating
    ,   NS "chromenspwrk" chromenspWrkCommand isChromenspWrk nonFloating

    ,   NS "discord"  discordCommand isDiscord defaultFloating
    ,   NS "youtubeMusic"  youtubeMusicCommand isYoutubeMusic nonFloating
    ,   NS "calc"  "gnome-calculator --class=calcu" (className =? "calcu") nonFloating

    ,   NS "console"  "kitty --class=kittyconsole" (className =? "kittyconsole") nonFloating
    ]

----------------------------------------------------------------------------------------------------
-- Theme                                                                                     --
----------------------------------------------------------------------------------------------------

-- At the moment this theme is an attempt at a TokyoNight colour pallet. There might still be some
-- leftovers of my Monokai theme from when I used that. I moved not for looks, but because of the
-- amazing functionality of Folke's Neovim theme.

myFocusFollowsMouse  = True
myClickJustFocuses   = True

background = "#1a1b26"
foreground = "#a9b1d6"
dull       = "#565f89"
active     = "#7595E0"
visible    = "#9ece6a"
warning    = "#e0af68"
alert      = "#f7768e"

-- sizes
gap    = 4
reSize = 1/20

topbar   = 8
tabsh    = 20
myBorder = 3
prompt   = 30

myNormalBorderColor     = background
myFocusedBorderColor    = active

myFont      = "xft:Ubuntu:weight=normal:pixelsize=16:antialias=true:hinting=true"
myWideFont  = "xft:Eurostar Black Extended:"
            ++ "style=Regular:pixelsize=180:hinting=true"

-- This is a "fake title" used as a highlight bar in lieu of full borders I'm not a fan of it, so
-- it's not being used right now I take that back, spent a short time with a border, but now I've
-- moved back to the top bar, it works a lot better without rounded corners although part of me
-- so wishes to return to rounded corners. I think some of these could be cleaned up, especially
-- because I don't use the urgency hooks, so I don't ever get the urgency colours.

topBarTheme :: Theme
topBarTheme = def
    { fontName              = myFont
    , inactiveBorderColor   = background
    , inactiveColor         = background
    , inactiveTextColor     = background
    , activeBorderColor     = active
    , activeColor           = active
    , activeTextColor       = active
    , urgentBorderColor     = alert
    , urgentTextColor       = warning
    , decoHeight            = topbar
    }

topFloatBarTheme :: Theme
topFloatBarTheme = topBarTheme
    { fontName              = myFont
    , inactiveBorderColor   = background
    , inactiveColor         = background
    , inactiveTextColor     = foreground
    , activeBorderColor     = background
    , activeColor           = background
    , activeTextColor       = active
    , urgentBorderColor     = alert
    , urgentTextColor       = warning
    , decoHeight            = tabsh
    }

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

-- Honestly just my favourite section. I like things to just be exactly where I want them to, so
-- having custom layouts is what brought me to XMonad it the first place. The Notebook Layout is my
-- baby, it gets it's name from the fact that I originally made it to have a jupyter notebook next
-- to my text editor and browser. Even with that not being a use case for me any more, I still love
-- it so much. It has now been generalised to work for more use cases and can be of much use for
-- very wide monitors.

-- Tell X.A.Navigation2D about specific layouts and how to handle them
-- Don't actually know what this does, but removing it breaks things

myNav2DConf :: Navigation2DConfig
myNav2DConf = def
    { defaultTiledNavigation    = centerNavigation
    , floatNavigation           = centerNavigation
    , screenNavigation          = lineNavigation
    , layoutNavigation          = [("Full", centerNavigation)]
    , unmappedWindowRect        = [("Full", singleWindowRect)]
    }

-- This was in all the layout functions, might as well have it in one place
mySpacing = spacingRaw False (Border gap gap gap gap) True (Border gap gap gap gap) True

-- To make a toggle for full-screening a program but leaving the bar in place. Also changes the top
-- bar to warning to remind me that there are probably other programs open.
data FULLBAR = FULLBAR deriving (Read, Show, Eq, Typeable)
instance Transformer FULLBAR Window where
    transform FULLBAR x k = k barFull (const x)

barFull = renamed [Replace "Maximized"]
        $ noFrillsDeco shrinkText topBarTheme
        $ avoidStruts
        $ addTabs shrinkText myTabTheme
        $ mySpacing
          Simplest

-- To make a toggle for Zen. Centres the active window and hides everything else except the bar. 
-- Also sets the top bar to warning to remind me that other programs are probably still open.
data FULLCENTER = FULLCENTER deriving (Read, Show, Eq, Typeable)
instance Transformer FULLCENTER Window where
    transform FULLCENTER x k = k centerFull (const x)

centerFull = renamed [Replace "Centred Max"]
           $ noFrillsDeco shrinkText topBarTheme
           $ avoidStruts
           $ addTabs shrinkText myTabTheme
           $ mySpacing
           $ onWorkspaces [wsTHESIS] (SimpleFocus (1/4) (reSize/2) 1000)
           $ SimpleFocus (1/2) (reSize/2) 1500

-- cf http://xmonad.org/xmonad-docs/xmonad-contrib/src/XMonad-Config-Droundy.html
myLayoutHook= onWorkspaces [wsFLOAT] floatWorkSpace
            $ noBorders
            $ windowNavigation
            $ fullScreenToggle
            $ fullBarToggle
            $ fullCenterToggle
            $ renamed [CutWordsLeft 4]
            $ showWorkspaceName
            $ windowSwitcherDecoration shrinkText topBarTheme
            $ draggingVisualizer
            $ addTabs shrinkText myTabTheme
            $ avoidStruts
            $ mySpacing
            $ mirrorToggle
            $ reflectToggle
            $ notebookLayout  ||| tabsLayout
    where
    notebookMulti   = subLayout [] Simplest $ Notebook 2560 True True True 1 3 reSize 2 (2/3)
    notebookColumns = subLayout [] Simplest $ Notebook 1920 False True True 3 3 reSize 2 (2/3)

    notebookLayout = renamed [Replace "Normal"] $ onWorkspaces [wsTMP, wsTMP2, wsPER, wsWRK] notebookColumns notebookMulti

    tabsLayout = renamed [Replace "Tall Tabs"] (mastered (1/100) (1/2) Simplest)

    floatWorkSpace = renamed [Replace "Float"] (borderResize $ addFloatTopBar positionStoreFloat)

    -- Other Layout Stuff
    fullBarToggle       = mkToggle (single FULLBAR)
    fullCenterToggle    = mkToggle (single FULLCENTER)
    fullScreenToggle    = mkToggle (single FULL)
    mirrorToggle        = mkToggle (single MIRROR)
    reflectToggle       = mkToggle (single REFLECTX)
    showWorkspaceName   = showWName' myShowWNameTheme
    addFloatTopBar      = noFrillsDeco shrinkText topFloatBarTheme

----------------------------------------------------------------------------------------------------
-- Keybindings                                                                                    --
----------------------------------------------------------------------------------------------------

-- The general philosophy was that all WM bindings would be on the Super key, and no other bindings
-- would go there. Some exceptions have been made. F8 is used as my keyboard is set up so that a tap
-- of the Super key actually sends the F8 key (It can't do modified key presses with that function).
-- And also some more keys are creeping onto the Super layer in the form of 'magic' bindings.
-- Bindings that try to bridge the gap between the XMonad and the window and tab management within
-- certain programs.

myModMask :: KeyMask
myModMask = mod4Mask -- super (and on my system, hyper) keys

-- Display keyboard mappings using zenity.
-- from: github.com/thomasf/dotfiles-thomasf-xmonad/blob/master/.xmonad/lib/XMonad/Config/A00001.hs

showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings x = addName "Show Keybindings" $ io $ do
    h <- spawnPipe "zenity --text-info --font=terminus"
    hPutStr h (unlines $ showKm x)
    hClose h
    return ()

-- some of the structure of the following cribbed from
-- https://github.com/SimSaladin/configs/blob/master/.xmonad/xmonad.hs
-- https://github.com/paul-axe/dotfiles/blob/master/.xmonad/xmonad.hs
-- https://github.com/pjones/xmonadrc (+ all the dyn project stuff)

myKeys :: XConfig Layout -> [((KeyMask, KeySym), NamedAction)]
myKeys conf = let

    subKeys str ks = subtitle str : mkNamedKeymap conf ks
    -- Added home and end for the two starting workspaces 
    -- (This makes sense on my batshit keyboard layout)
    wsKeys = ["S-#", "#", "1", "2", "3", "4", "5", "6", "7", "8", "9", "0"]

    zipM  m nm ks as f = zipWith (\k d -> (m ++ k, addName nm $ f d)) ks as

    swapMaster' (W.Stack f u d) = W.Stack f [] $ reverse u ++ d

    warpCursor = warpToWindow (1/2) (1/2)

    toggleFloat w = windows (\s -> if M.member w (W.floating s)
                    then W.sink w s
                    else W.float w (W.RationalRect (1/4) (1/4) (1/2) (1/2)) s)

    in

    subKeys "System"
    [ ("M-q"             , addName "Restart XMonad"              $ spawn "xmonad --restart")
    , ("M-C-q"           , addName "Rebuild & restart XMonad"    $ spawn "xmonad --recompile && xmonad --restart")
    , ("M-S-q"           , addName "Quit XMonad"                 $ confirmPrompt hotPromptTheme "Quit XMonad" $ io exitSuccess)
    , ("M-M1-C-S-x"      , addName "Lock screen"                 $ spawn myLockscreen)
    , ("M-x"             , addName "notification panel"          $ spawn "toggle notif")
    ] ^++^

    subKeys "Utilities"
    [ ("M-M1-C-S-z"      , addName "Colour picker"               $ spawn myColorPicker) -- kill picom before use
    , ("M-M1-C-S-o"      , addName "On-screen keys"              $ spawn "killall screenkey || screenkey")
    , ("M-M1-C-S-/"      , addName "On-screen keys settings"     $ spawn "screenkey --show-settings")
    , ("M-M1-C-S-f"      , addName "Capture screen"              $ spawn "screencapt" )
    , ("M-M1-C-S-s"      , addName "Capture selection"           $ spawn "screencapt area" )
    , ("M-M1-C-S-w"      , addName "Record screen"               $ spawn "screencast" )
    , ("M-M1-C-S-r"      , addName "Record area"                 $ spawn "screencast area" )
    , ("M-M1-C-S-<Space>", addName "Play/Pause"                  $ spawn "playerctl play-pause" )
    , ("M-M1-C-S-<Up>"   , addName "Skip Song"                   $ spawn "playerctl previous" )
    , ("M-M1-C-S-<Down>" , addName "Prev Song"                   $ spawn "playerctl next" )
    , ("M-M1-C-S-<Left>" , addName "Skip Song"                   $ spawn "playerctl previous" )
    , ("M-M1-C-S-<Right>", addName "Prev Song"                   $ spawn "playerctl next" )
    , ("M-;"             , addName "Warp Cursor"                 $ warpToWindow (1/2) (1/2))
    ] ^++^

    subKeys "Apps"
    [ ("M-<Return>"      , addName "Terminal"                    $ bindFirst [(className =? "kitty", P.sendKey (controlMask .|. shiftMask) xK_Return)
                                                                             ,(className =? "kittyconsole", P.sendKey (controlMask .|. shiftMask) xK_Return)
                                                                             ,(pure True, sequence_ [runOrRaise myTerminal (className =? "kitty"), warpCursor])]) -- P.sendKey (controlMask .|. shiftMask) xK_Return])])
    , ("M-C-<Return>"    , addName "Force Terminal"              $ spawn myTerminal)
    , ("M-b"             , addName "Browser"                     $ bindFirst [(className =? "Google-chrome", P.sendKey controlMask xK_t)
                                                                             ,(pure True, sequence_ [runOrRaise myBrowser (className =? "Google-chrome"), warpCursor])]) -- P.sendKey controlMask xK_t])])
    , ("M-v"             , addName "PDF Viewer"                  $ sequence_ [runOrRaise "zathura" (className =? "Zathura"), warpCursor])
    , ("M-C-b"           , addName "Force Browser"               $ spawn myBrowser)
    , ("M-M1-b"          , addName "Work Browser"                $ bindOn WS [(wsTMP2,   spawn "google-chrome-stable --user-data-dir='/home/oleete/.config/browser/google-chrome-stable'")
                                                                             ,(wsTMP,    spawn "google-chrome-stable --user-data-dir='/home/oleete/.config/browser/google-chrome-stable'")
                                                                             ,(wsWRK,    spawn "google-chrome-stable --user-data-dir='/home/oleete/.config/browser/google-chrome-stable'")
                                                                             ,(wsWRK4,   spawn "google-chrome-stable --user-data-dir='/home/oleete/.config/browser/google-chrome-stable'")
                                                                             ,(wsTHESIS, spawn "google-chrome-stable --user-data-dir='/home/oleete/.config/browser/google-chrome-stable'")
                                                                             ,(wsEXP,    spawn "google-chrome-stable --user-data-dir='/home/oleete/.config/browser/google-chrome-stable'")
                                                                             ,(wsSIM,    spawn "google-chrome-stable --user-data-dir='/home/oleete/.config/browser/google-chrome-stable'")
                                                                             ,("",       spawn "google-chrome-stable --user-data-dir='/home/oleete/.config/browser/google-chrome-stable-wrk'")])
    , ("M-e"             , addName "Explorer"                    $ bindOn WS [(wsSIM,  spawn (myExplorer ++ " -t ~/Projects/JuliaPowderModel ~/UniDrive/1_Thesis/1.4_PowderModel"))
                                                                             ,(wsEXP,  spawn (myExplorer ++ " -t ~/Projects/JuliaPlotting ~/UniDrive/1_Thesis/1.4_PowderModel"))
                                                                             ,(wsPRO2, spawn (myExplorer ++  " -t ~/Projects/julia-vscode ~/Projects/julia-benchmark-example"))
                                                                             ,(wsCON,  spawn (myExplorer ++  " -t ~/Projects/Configs ~/Projects/ConfigExamples ~/.config"))
                                                                             ,(wsPRO1,  spawn (myExplorer ++  " -t ~/Projects/Prints/stl ~/Projects/Prints/gcode"))
                                                                             ,(wsPER,  spawn (myExplorer ++  " -t ~/Downloads ~/PersonalDrive"))
                                                                             ,(wsWRK,  spawn (myExplorer ++  " -t ~/Downloads ~/UniDrive"))
                                                                             ,("",     spawn (myExplorer ++ " ."))])
    ] ^++^

    subKeys "ScratchPad Apps"
    [ ("M-S-c"           , addName "NSP Calculator"              $ namedScratchpadAction scratchpads "calc")
    , ("M-S-<Return>"    , addName "NSP Console"                 $ namedScratchpadAction scratchpads "console")
    , ("M-S-d"           , addName "NSP Discord"                 $ namedScratchpadAction scratchpads "discord")
    , ("M-S-m"           , addName "NSP Music"                   $ namedScratchpadAction scratchpads "youtubeMusic")
    , ("M-S-b"           , addName "NSP Browser"                 $ bindOn WS [(wsWRK, namedScratchpadAction scratchpads "chromenspwrk")
                                                                             ,(wsSIM, namedScratchpadAction scratchpads "chromenspwrk")
                                                                             ,(wsEXP, namedScratchpadAction scratchpads "chromenspwrk")
                                                                             ,(wsTHESIS, namedScratchpadAction scratchpads "chromenspwrk")
                                                                             ,(wsWRK4, namedScratchpadAction scratchpads "chromenspwrk")
                                                                             ,("", namedScratchpadAction scratchpads "chromensp")])
    , ("M-S-t"           , addName "NSP Tasks"                   $ bindOn WS [(wsWRK, namedScratchpadAction scratchpads "tasksWork")
                                                                             ,(wsSIM, namedScratchpadAction scratchpads "tasksWork")
                                                                             ,(wsEXP, namedScratchpadAction scratchpads "tasksWork")
                                                                             ,(wsTHESIS, namedScratchpadAction scratchpads "tasksWork")
                                                                             ,(wsWRK4, namedScratchpadAction scratchpads "tasksWork")
                                                                             ,("", namedScratchpadAction scratchpads "tasks")])
    , ("M-S-n"           , addName "NSP Keep"                    $ bindOn WS [(wsWRK, namedScratchpadAction scratchpads "keepWrkNsp")
                                                                             ,(wsSIM, namedScratchpadAction scratchpads "keepWrkNsp")
                                                                             ,(wsEXP, namedScratchpadAction scratchpads "keepWrkNsp")
                                                                             ,(wsTHESIS, namedScratchpadAction scratchpads "keepWrkNsp")
                                                                             ,(wsWRK4, namedScratchpadAction scratchpads "keepWrkNsp")
                                                                             ,("", namedScratchpadAction scratchpads "keepNsp")])
    ] ^++^

    subKeys "Workspaces and Projects"
    (
    [ ("M-a"             , addName "Launcher"                    $ spawn myLauncher)

    -- , ("<F8>"            , addName "prompt select window"        $ windowPrompt myPromptTheme Goto allWindows)
    -- , ("C-<F8>"          , addName "prompt fetch window"         $ windowPrompt myPromptTheme Bring allWindows)
    , ("M-w"             , addName "prompt select ws"            $ switchProjectPrompt myPromptTheme)
    , ("M-C-w"           , addName "prompt send to ws"           $ shiftToProjectPrompt myPromptTheme)
    ]
    ++ zipM "M-"         "View ws"                               wsKeys [0..] (withNthWorkspace W.greedyView)
    ++ zipM "M-C-"       "Move w to ws"                          wsKeys [0..] (withNthWorkspace W.shift)
    ) ^++^

    subKeys "Windows"
    [ ("M-<Backspace>"   , addName "Kill"                        $ bindFirst [(className =? "kitty", P.sendKey (controlMask .|. shiftMask) xK_F12)
                                                                             ,(className =? "kittyconsole", P.sendKey (controlMask .|. shiftMask) xK_F12)
                                                                             ,(className =? "Google-chrome", P.sendKey controlMask xK_w)
                                                                             ,(pure True, kill)])
    , ("M-C-<Backspace>" , addName "Force kill"                  kill)
    , ("M-S-<Backspace>" , addName "Kill all"                    $ confirmPrompt hotPromptTheme "kill all" killAll)

    , ("M-m"             , addName "Swap with main"              $ sequence_ [swapPromote' False, warpCursor])
    , ("M-C-m"           , addName "Promote to main"             $ sequence_ [promote, warpCursor])

    , ("<F8>"            , addName "Hop to Window"               $ sequence_ [selectWindow easymotionConfig >>= (`whenJust` windows . W.focusWindow), warpCursor])

    , ("M-<Space>"       , addName "Swap monitor workspaces"     swapNextScreen)
    , ("M-C-<Space>"     , addName "Send window to next monitor" shiftNextScreen)

    , ("M-<D>"           , addName "Focus down"                  $ bindFirst [(className =? "kitty", P.sendKey (controlMask .|. shiftMask) xK_Down)
                                                                             ,(className =? "kittyconsole", P.sendKey (controlMask .|. shiftMask) xK_Down)
                                                                             ,(pure True, sequence_ [windows W.focusDown, warpCursor])])
    , ("M-<U>"           , addName "Focus up"                    $ bindFirst [(className =? "kitty", P.sendKey (controlMask .|. shiftMask) xK_Up)
                                                                             ,(className =? "kittyconsole", P.sendKey (controlMask .|. shiftMask) xK_Up)
                                                                             ,(pure True, sequence_ [windows W.focusUp, warpCursor])])
    , ("M-C-<D>"         , addName "Force Focus down"            $ sequence_ [windows W.focusDown, warpCursor])
    , ("M-C-<U>"         , addName "Force Focus up"              $ sequence_ [windows W.focusUp, warpCursor])

    , ("M-S-<D>"         , addName "Shift down"                  $ sequence_ [windows W.swapDown, warpCursor])
    , ("M-S-<U>"         , addName "Shift up"                    $ sequence_ [windows W.swapUp, warpCursor])

    , ("M-<R>"           , addName "Cycle up"                    $ bindFirst [(className =? "kitty", P.sendKey (controlMask .|. shiftMask) xK_Right)
                                                                             ,(className =? "kittyconsole", P.sendKey (controlMask .|. shiftMask) xK_Right)
                                                                             ,(className =? "Google-chrome", P.sendKey controlMask xK_Tab)
                                                                             ,(pure True, bindOn LD [("Tall Tabs", rotSlavesUp), ("Tabs", windows W.focusDown), ("Maximized", windows W.focusDown), ("Centred Max", windows W.focusDown), ("", onGroup W.focusDown')])])

    , ("M-<L>"           , addName "Cycle down"                  $ bindFirst [(className =? "kitty", P.sendKey (controlMask .|. shiftMask) xK_Left)
                                                                             ,(className =? "kittyconsole", P.sendKey (controlMask .|. shiftMask) xK_Left)
                                                                             ,(className =? "Google-chrome", P.sendKey (controlMask .|. shiftMask) xK_Tab)
                                                                             ,(pure True, bindOn LD [("Tall Tabs", rotSlavesDown), ("Tabs", windows W.focusUp), ("Maximized", windows W.focusUp), ("Centred Max", windows W.focusUp), ("", onGroup W.focusDown')])])
    , ("M-C-<R>"         , addName "Force Cycle up"              $ bindOn LD [("Tall Tabs", rotSlavesUp), ("Tabs", windows W.focusDown), ("Maximized", windows W.focusDown), ("Centred Max", windows W.focusDown), ("", onGroup W.focusDown')])
    , ("M-C-<L>"         , addName "Force Cycle down"            $ bindOn LD [("Tall Tabs", rotSlavesDown), ("Tabs", windows W.focusUp), ("Maximized", windows W.focusUp), ("Centred Max", windows W.focusUp), ("", onGroup W.focusDown')])

    , ("M-t"             , addName "New Tab"                     $ bindFirst [(className =? "kitty", P.sendKey (controlMask .|. shiftMask) xK_t)
                                                                             ,(className =? "kittyconsole", P.sendKey (controlMask .|. shiftMask) xK_t)
                                                                             ,(className =? "Google-chrome", P.sendKey controlMask xK_t)])


    , ("M-f"             , addName "Fullscreen"                  $ sequence_ [ withFocused $ windows . W.sink
                                                                 , sendMessage $ XMonad.Layout.MultiToggle.Toggle FULL ])

    , ("M-s"             , addName "Maximize"                    $ sequence_ [ withFocused $ windows . W.sink
                                                                 , sendMessage $ XMonad.Layout.MultiToggle.Toggle FULLBAR ])

    , ("M-c"             , addName "Center Focus"                $ sequence_ [ withFocused $ windows . W.sink
                                                                 , sendMessage $ XMonad.Layout.MultiToggle.Toggle FULLCENTER])


    , ("M-u"             , addName "Scroll Up"                   $ spawn "xdotool click 4; sleep 0.001; xdotool click 4; sleep 0.001; xdotool click 4; sleep 0.001; xdotool click 4; sleep 0.001; xdotool click 4; sleep 0.001; xdotool click 4")
    , ("M-d"             , addName "Scroll Down"                 $ spawn "xdotool click 5; sleep 0.001; xdotool click 5; sleep 0.001; xdotool click 5; sleep 0.001; xdotool click 5; sleep 0.001; xdotool click 5; sleep 0.001; xdotool click 5")
    ] ^++^

    subKeys "SubLayouts"
    [ ("M-M1-m"          , addName "SubLayout swapMain"          $ onGroup swapMaster')
    , ("M-M1-<Tab>"      , addName "Cycle sublayout"             $ toSubl NextLayout)
    , ("M-M1-<D>"        , addName "SubLayout combine down"      $ withFocused (sendMessage . mergeDir W.focusDown'))
    , ("M-M1-<U>"        , addName "SubLayout combine up"        $ withFocused (sendMessage . mergeDir W.focusUp'))

    , ("M-i"             , addName "Un-merge from sublayout"     $ withFocused (sendMessage . UnMerge))
    , ("M-C-i"           , addName "Unmerge all from sublayout"  $ withFocused (sendMessage . UnMergeAll))
    , ("M-M1-i"          , addName "Unmerge all from sublayout"  $ withFocused (sendMessage . UnMergeAll))

    , ("M-M1-h"          , addName "Merge Left"                  $ sequence_ [sendMessage $ pullGroup L, warpCursor])
    , ("M-M1-j"          , addName "Merge Down"                  $ sequence_ [sendMessage $ pullGroup D, warpCursor])
    , ("M-M1-k"          , addName "Merge Up"                    $ sequence_ [sendMessage $ pullGroup U, warpCursor])
    , ("M-M1-l"          , addName "Merge Right"                 $ sequence_ [sendMessage $ pullGroup R, warpCursor])

    , ("M-C-M1-h"        , addName "Send Left"                   $ sequence_ [sendMessage $ pushWindow L, warpCursor])
    , ("M-C-M1-j"        , addName "Send Down"                   $ sequence_ [sendMessage $ pushWindow D, warpCursor])
    , ("M-C-M1-k"        , addName "Send Up"                     $ sequence_ [sendMessage $ pushWindow U, warpCursor])
    , ("M-C-M1-l"        , addName "Send Right"                  $ sequence_ [sendMessage $ pushWindow R, warpCursor])
    ] ^++^

    subKeys "Layout Management"
    [ ("M-<Tab>"         , addName "Cycle all layouts"           $ sendMessage NextLayout)
    , ("M-S-<Tab>"       , addName "Reset layout"                $ setLayout $ XMonad.layoutHook conf)
    , ("M-C-<Tab>"       , addName "Toggle sublayout"            $ bindOn LD [("Normal", sendMessage ToggleMiddle)
                                                                             ,("Columns", sendMessage ToggleMiddle)
                                                                             ,("", sendMessage ToggleLayout)])

    , ("M-y"             , addName "Toggle window floating"      $ withFocused toggleFloat)
    , ("M-C-y"           , addName "Tile all floating w"         sinkAll)

    , ("M-,"             , addName "Decrease main windows"       $ sendMessage (IncMasterN (-1)))
    , ("M-."             , addName "Increase main windows"       $ sendMessage (IncMasterN 1))
    , ("M-C-,"           , addName "Decrease big windows"        $ sendMessage (IncColumnN (-1)))
    , ("M-C-."           , addName "Increase big windows"        $ sendMessage (IncColumnN 1))

    , ("M-["             , addName "Shrink Main"                 $ sendMessage Shrink)
    , ("M-]"             , addName "Expand Main"                 $ sendMessage Expand)
    , ("M-C-["           , addName "Shrink height"               $ sendMessage MirrorShrink)
    , ("M-C-]"           , addName "Expand height"               $ sendMessage MirrorExpand)

    , ("M-r"             , addName "Reflect"                     $ bindOn LD [("Normal",sendMessage ToggleSide)
                                                                             ,("", sendMessage (XMonad.Layout.MultiToggle.Toggle REFLECTX))])
    , ("M-C-r"           , addName "Reflect Stack"               $ sendMessage ToggleStackDir)
    ]

-- Mouse bindings: default actions bound to mouse events. Includes window w/h ratio constraint
-- (square) using X.H.ConstrainedResize. cleared these down a bit as I don't use floating windows
-- often
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

-- Things to do on startup. Spawn is used instead of spawnOnce for wallpaper to give a new random on
-- every reset of XMonad.

myStartupHook :: X ()
myStartupHook = do
    spawnOnce myCompositor
    spawn myWallpaper
    spawnOnce myTray
    spawnOnce "insync start; insync hide"
    spawnOnce "xsetroot -cursor_name left_ptr"
    spawnOnce "deadd-notification-center &"


----------------------------------------------------------------------------------------------------
-- Log                                                                                            --
----------------------------------------------------------------------------------------------------

-- Here is all the stuff to make my pp look nice. And by this, I mean it's the things used to put
-- the workspace names in xmobar.

myLogHook :: Handle -> X ()
myLogHook h = do
    fadeWindowsLogHook myFadeHook
    ewmhDesktopsLogHook
    masterHistoryHook
    dynamicLogWithPP . filterOutWsPP ["NSP"] $ def
        { ppCurrent             = xmobarColor active "" . wrap "[" "]" . clickable
        , ppTitle               = xmobarColor active "" . wrap "<action=xdotool key Super+s>" "</action>" . shorten 20
        , ppVisible             = xmobarColor visible  "" . clickable
        , ppUrgent              = xmobarColor alert    "" . wrap "!" "!"
        , ppHidden              = xmobarColor dull  "" . clickable
        , ppHiddenNoWindows     = const ""
        , ppSep                 = " | "
        , ppWsSep               = " | "
        , ppLayout              = xmobarColor warning "" . wrap "<action=xdoforegroundtool key Super+Tab>" "</action>"
        , ppOrder               = id
        , ppOutput              = hPutStrLn h
        , ppSort                = ppSort def
        , ppExtras              = [willHookNextPP "Main" $ xmobarColor alert "> Place in Main <fn=1></fn"] }


myFadeHook :: FadeHook
myFadeHook = composeAll
    [ opaque -- default to opaque
    , isUnfocused --> opacity 0.90
    -- , (className =? "kitty") <&&> isUnfocused --> opacity 0.90
    , className =? "deadd-notification-center" --> opaque
    , fmap ("Google" `isPrefixOf`) className <&&> isUnfocused --> opacity 1
    , isDialog --> opaque
    , isFloating  --> opacity 1
    ]

----------------------------------------------------------------------------------------------------
-- New Window Actions                                                                             --
----------------------------------------------------------------------------------------------------

-- Things to do when a new window is opened. Mostly just making the named scratchpad apps open
-- floating in rectangles. Also adds the main toggle, this one shot toggle puts the next created
-- window in the master spot. This sometimes useful, but I normally like new windows spawning at
-- the bottom of the stack (I often just have the one or two 'main' programs that I want on each
-- workspace, and new windows after that are just supporting windows)

-- https://wiki.haskell.org/Xmonad/General_xmonad.hs_config_tips#ManageHook_examples

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
            , className =? "Chromensp" -?> doRectFloat (W.RationalRect (1 / 4) (1 / 8) (1 / 2) (3 / 4))
            , className =? "Chromewrknsp" -?> doRectFloat (W.RationalRect (1 / 4) (1 / 8) (1 / 2) (3 / 4))
            , resource =? youtubeMusicResource -?> doRectFloat halfNhalf
            , resource =? discordResource -?> doRectFloat halfNhalf

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

-- I'm not really into full screens without my say so... I often like to fullscreen a window but
-- keep it constrained to a window rect (e.g. for videos, etc. without the UI chrome cluttering
-- things up). I can always do that and then full screen the subsequent window if I want. THUS, to
-- cut a long comment short, no fullscreenEventHook

myHandleEventHook :: Event -> X All
myHandleEventHook = docksEventHook
                <+> fadeWindowsEventHook
                <+> handleEventHook def
                <+> XMonad.Util.Hacks.windowedFullscreenFixEventHook

----------------------------------------------------------------------------------------------------
-- Custom hook helpers                                                                            --
----------------------------------------------------------------------------------------------------

-- from:
-- https://github.com/pjones/xmonadrc/blob/master/src/XMonad/Local/Action.hs
--
-- Useful when a floating window requests stupid dimensions. There was a bug in Handbrake that would
-- pop up the file dialog with almost no height due to one of my rotated monitors.

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
