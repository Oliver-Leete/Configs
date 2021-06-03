{-# LANGUAGE AllowAmbiguousTypes, DeriveDataTypeable, TypeSynonymInstances, MultiParamTypeClasses #-}
----------------------------------------------------------------------------------------------------
--                                                                                                --
--                        __   __  __  __                               _                         --
--                        \ \ / / |  \/  |                             | |                        --
--                         \ V /  | \  / |   ___    _ __     __ _    __| |                        --
--                          > <   | |\/| |  / _ \  | '_ \   / _` |  / _` |                        --
--                         / . \  | |  | | | (_) | | | | | | (_| | | (_| |                        --
--                        /_/ \_\ |_|  |_|  \___/  |_| |_|  \__,_|  \__,_|                        --
----------------------------------------------------------------------------------------------------
-- Oliver Leete <oliverleete@gmail.com>                                                           --
-- https://github.com/altercation                                                                 --
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
import Data.Monoid
import System.Exit
import System.IO

import XMonad hiding ( (|||) )
import qualified XMonad.StackSet as W

import XMonad.Actions.ConditionalKeys
import qualified XMonad.Actions.ConstrainedResize as Sqr
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicProjects
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.Navigation2D
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves
import XMonad.Actions.SinkAll
import XMonad.Actions.SpawnOn
import XMonad.Actions.SwapPromote
import XMonad.Actions.WithAll

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.DynamicProperty
import XMonad.Hooks.EwmhDesktops ( ewmhDesktopsLogHook, ewmh )
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ToggleHook
import XMonad.Hooks.UrgencyHook

import XMonad.Layout.BorderResize
import XMonad.Layout.DraggingVisualizer
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.OneBig
import XMonad.Layout.PerScreen
import XMonad.Layout.PerWorkspace
import XMonad.Layout.PositionStoreFloat
import XMonad.Layout.Reflect
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.ShowWName
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.WindowNavigation
import XMonad.Layout.WindowSwitcherDecoration

import XMonad.Layout.FourColumns
-- import XMonad.Layout.CenterFocus
import XMonad.Layout.Notebook

import XMonad.Prompt
import XMonad.Prompt.AppLauncher as AL
import XMonad.Prompt.ConfirmPrompt
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.Window

import XMonad.Util.EZConfig
import XMonad.Util.Hacks
import XMonad.Util.NamedActions
import XMonad.Util.NamedScratchpad
import XMonad.Util.NamedWindows
import XMonad.Util.Run
import XMonad.Util.SpawnOnce
import XMonad.Util.WorkspaceCompare()

-- experimenting with tripane
-- import XMonad.Layout.Decoration
-- import XMonad.Hooks.WindowSwallowing

-- testing
import XMonad.Layout.Column
import XMonad.Layout.SimpleFocus
import XMonad.Layout.Master
import XMonad.Actions.Warp
import XMonad.Util.Paste as P
-- NOTE: Try out prompt with submap
import Data.Maybe

-- NOTE: try out XMonad.Actions.Submap

----------------------------------------------------------------------------------------------------
-- Main                                                                                           --
----------------------------------------------------------------------------------------------------

main :: IO ()
main = do
    xmproc0 <- spawnPipe myStatusBar

    xmonad
        $ dynamicProjects projects
        $ withNavigation2DConfig myNav2DConf
        $ withUrgencyHook LibNotifyUrgencyHook
        $ ewmh
        $ addDescrKeys' ((myModMask, xK_F1), showKeybindings) myKeys
        $ myConfig xmproc0

myConfig p = def
        { borderWidth        = myborder
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
-- are inbetween the two. The Start and Gen workspaces are hidden elsewhere mostly as placeholders
-- so no applications are run at start up, with the bonus of having nice wallpapers without them
-- being destracting whilst I work

wsTMP    = "tmp"
wsTMP2    = "tmp2"
wsPRO1   = "pro1 - home"
wsPRO2   = "extension - home"
wsPRO3   = "tutorils - home"
wsCON    = "configs - home"
wsPER    = "home"
wsFLOAT  = "flt"
wsWRK    = "wrk"
wsSIM    = "sim - wrk"
wsTHESIS = "thesis - wrk"
wsEXP    = "experiments - wrk"
wsWRK4   = "wrk4"

wsDND    = "dnd"
wsDND2   = "dnd2"

myWorkspaces :: [[Char]]
myWorkspaces = [wsTMP, wsTMP2, wsPRO1, wsPRO2, wsPRO3, wsCON, wsPER, wsWRK, wsSIM, wsEXP, wsTHESIS, wsWRK4, wsFLOAT, wsDND, wsDND2]

myWorkspaceIndices :: M.Map [Char] [Char]
myWorkspaceIndices = M.fromList $ zip myWorkspaces ["<Home>", "<End>", "1", "2", "3", "4", "5", "6", "7", "8", "9", "0"] -- (,) == \x y -> (x,y)

clickable :: [Char] -> [Char]
clickable ws = "<action=xdotool key super+"++show i++">"++ws++"</action>"
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
                , projectDirectory  = "~/Projects"
                , projectStartHook  = Just $ do spawnOn wsPRO1 ("sleep .3; " ++ myBrowser)
                }
    , Project   { projectName       = wsPRO2
                , projectDirectory  = "~/Projects/julia-vscode"
                , projectStartHook  = Just $ do spawnOn wsPRO2 (myEditor ++ " .")
                                                spawnOn wsPRO2 ("sleep .3; " ++ myBrowser)
                }

    , Project   { projectName       = wsPRO3
                , projectDirectory  = "~/Projects/JuliaTutorials"
                , projectStartHook  = Just $ do spawnOn wsPRO3 (myNotebook ++ " .")
                                                spawnOn wsPRO3 ("sleep .5; " ++ myBrowser)
                }

    , Project   { projectName       = wsCON
                , projectDirectory  = "~/.config"
                , projectStartHook  = Just $ do spawnOn wsCON (myTerminal ++ " --session=/home/oleete/.config/kitty/config.conf")
                                                spawnOn wsCON ("sleep 1.5; " ++ myBrowser)
                }

    , Project   { projectName       = wsPER
                , projectDirectory  = "~/PersonalDrive"
                , projectStartHook  = Just $    spawnOn wsPER myBrowser
                }

    , Project   { projectName       = wsWRK
                , projectDirectory  = "~/UniDrive"
                , projectStartHook  = Just $    spawnOn wsWRK myBrowser
                }

    , Project   { projectName       = wsSIM
                , projectDirectory  = "~/Projects/JuliaPowderModel"
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
    , Project   { projectName       = wsDND
                , projectDirectory  = "~/Projects/D&D Home"
                , projectStartHook  = Just $    spawnOn wsDND (myBrowser ++ " https://roll20.net/welcome")
                }

    ]

----------------------------------------------------------------------------------------------------
-- Applications                                                                                   --
----------------------------------------------------------------------------------------------------

-- Just a general list of all the apps I reference elsewhere

myTerminal     = "kitty"
myEditor       = "code-insiders"
myNotebook     = "code-insiders"
myBrowser      = "browser"
myBrowserClass = "google-chrome-stable"
myStatusBar    = "/home/oleete/.config/xmobar/init-xmobars"
myTray         = "trayer --edge bottom --align center --distance 5 --height 20 --widthtype request --transparent true --alpha 0 --tint 0x2c292d "
myLauncher     = "rofi -matching fuzzy -modi combi -show combi -combi-modi window,drun,run -show-icons"
myLockscreen   = "slock"
myExplorer     = "nemo"
myCompositor   = "picom -b --config ~/.config/picom/picom.conf"
myColorPicker  = "colorpicker"
-- Lets see if this fixes the below bug
myWallpaper    = "feh --bg-fill --randomize ~/Pictures/wallpapers/"

-- Ok, this gives the stranges bug. Sometimes on startup one of the two screens will have what seems
-- to be a dump of whatever is in the graphics memory. It is normally just static (sometimes a bit
-- blocky), but if I have recenty turned my computer off it will have splotches of what was on there
-- before.
--
-- myWallpaper    = "nitrogen --head=0 --random ~/Pictures/wallpapers/start/ --set-zoom-fill"
-- myWallpaper1   = "nitrogen --head=1 --random ~/Pictures/wallpapers/start/ --set-zoom-fill"

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

myFocusFollowsMouse  = True
myClickJustFocuses   = True

base03 = "#2C292D"
base02 = "#2C292D"
base00 = "#848B91"
base2  = "#FDF9F3"
yellow = "#ffd866"
orange = "#fc9867"
red    = "#ff6188"
green  = "#A9DC76"

-- sizes
gap    = 4
reSize = 1/40

topbar   = 8
tabsh    = 30
myborder = 3
prompt   = 30

myNormalBorderColor     = base03
myFocusedBorderColor    = yellow

active       = yellow

myFont      = "xft:Ubuntu:weight=normal:pixelsize=16:antialias=true:hinting=true"
myWideFont  = "xft:Eurostar Black Extended:"
            ++ "style=Regular:pixelsize=180:hinting=true"

-- This is a "fake title" used as a highlight bar in lieu of full borders I'm not a fan of it, so
-- it's not being used right now I take that back, spent a short time with a border, but now I've
-- moved back to the top bar, it works a lot better without rounded corners although part of me so
-- wishes to return to rounded corners
topBarTheme :: Theme
topBarTheme = def
    { fontName              = myFont
    , inactiveBorderColor   = base03
    , inactiveColor         = base03
    , inactiveTextColor     = base03
    , activeBorderColor     = active
    , activeColor           = active
    , activeTextColor       = active
    , urgentBorderColor     = red
    , urgentTextColor       = yellow
    , decoHeight            = topbar
    }

topMaxBarTheme :: Theme
topMaxBarTheme = topBarTheme
    { inactiveBorderColor   = orange
    , inactiveColor         = orange
    , inactiveTextColor     = orange
    , activeBorderColor     = orange
    , activeColor           = orange
    , activeTextColor       = orange
    , urgentBorderColor     = red
    , urgentTextColor       = red
    , decoHeight            = topbar
    }

topFloatBarTheme :: Theme
topFloatBarTheme = topBarTheme
    { fontName              = myFont
    , inactiveBorderColor   = base02
    , inactiveColor         = base02
    , inactiveTextColor     = base00
    , activeBorderColor     = base02
    , activeColor           = base02
    , activeTextColor       = active
    , urgentBorderColor     = red
    , urgentTextColor       = yellow
    , decoHeight            = tabsh
    }

myTabTheme :: Theme
myTabTheme = def
    { fontName              = myFont
    , activeColor           = base02
    , inactiveColor         = base02
    , activeBorderColor     = base02
    , inactiveBorderColor   = base02
    , activeTextColor       = active
    , inactiveTextColor     = base00
    , decoWidth             = 150
    , decoHeight            = tabsh
    }

myPromptTheme :: XPConfig
myPromptTheme = def
    { font                  = myFont
    , bgColor               = base02
    , fgColor               = base2
    , fgHLight              = yellow
    , bgHLight              = base02
    , borderColor           = yellow
    , promptBorderWidth     = 2
    , height                = prompt
    , position              = CenteredAt (1 / 4) (1 / 4)
    , searchPredicate       = fuzzyMatch
    , sorter                = fuzzySort
    , autoComplete          = Nothing
    }

hotPromptTheme :: XPConfig
hotPromptTheme = myPromptTheme
    { bgColor               = base02
    , fgColor               = red
    }

myShowWNameTheme :: SWNConfig
myShowWNameTheme = def
    { swn_font              = myWideFont
    , swn_fade              = 0.3
    , swn_bgcolor           = yellow
    , swn_color             = base03
    }

----------------------------------------------------------------------------------------------------
-- Layouts                                                                                        --
----------------------------------------------------------------------------------------------------

-- Tell X.A.Navigation2D about specific layouts and how to handle them

myNav2DConf :: Navigation2DConfig
myNav2DConf = def
    { defaultTiledNavigation    = centerNavigation
    , floatNavigation           = centerNavigation
    , screenNavigation          = lineNavigation
    , layoutNavigation          = [("Full", centerNavigation)]
    , unmappedWindowRect        = [("Full", singleWindowRect)]
    }

-- To make a toggle for fullscreening an app but leaving the bar in place
data FULLBAR = FULLBAR deriving (Read, Show, Eq, Typeable)
instance Transformer FULLBAR Window where
    transform FULLBAR x k = k barFull (const x)

barFull = renamed [Replace "Maximized"]
        $ noFrillsDeco shrinkText topMaxBarTheme
        $ avoidStruts
        $ spacingRaw False (Border gap gap gap gap) True (Border gap gap gap gap) True Simplest

-- To make a toggle pure zen. Centres the active window and hides everything else except the bar
data FULLCENTER = FULLCENTER deriving (Read, Show, Eq, Typeable)
instance Transformer FULLCENTER Window where
    transform FULLCENTER x k = k centerFull (const x)

centerFull = renamed [Replace "Centered Max"]
           $ noFrillsDeco shrinkText topMaxBarTheme
           $ avoidStruts
           $ spacingRaw False (Border gap gap gap gap) True (Border gap gap gap gap) True
           $ SimpleFocus (1/2) reSize

-- cf http://xmonad.org/xmonad-docs/xmonad-contrib/src/XMonad-Config-Droundy.html

myLayoutHook= onWorkspaces [wsFLOAT] floatWorkSpace
            $ noBorders
            $ fullScreenToggle
            $ fullBarToggle
            $ fullCenterToggle
            $ renamed [CutWordsLeft 4]
            $ showWorkspaceName
            $ windowSwitcherDecoration shrinkText topBarTheme $ draggingVisualizer
            $ addTabs shrinkText myTabTheme
            $ avoidStruts
            $ mySpacing
            $ mirrorToggle
            $ reflectToggle
            $ windowNavigation
            $ onWorkspaces [wsPER, wsWRK, wsWRK4] (oneBigLayout   ||| notebookLayout ||| threeColLayout ||| tabsLayout)
            $ onWorkspaces [wsTMP, wsTMP2]        (threeColLayout ||| tabsLayout     ||| oneBigLayout   ||| notebookLayout)
            $                                      notebookLayout ||| threeColLayout ||| tabsLayout     ||| oneBigLayout
    where
    notebookLayout = renamed [Replace "Notebook"] (subLayout [] Simplest
                   $ Notebook monResWidth True True True 1 3 (reSize*2) 2 (2/3))

    threeCol = renamed [Replace "Three Col"] (subLayout [] Simplest (FourCol True 1 reSize (51/100)))
    tabsThird = renamed [Replace "Third Tabs"] (mastered reSize (2/3) Simplest)
    columns = Mirror $ Column 1
    threeColLayout = ifWider smallMonResWidth threeCol (toggleLayouts tabsThird columns)

    tallTabs = renamed [Replace "Tall Tabs"] (mastered (1/100) (1/2) Simplest)
    allTabs = renamed [Replace "Tabs"] Simplest
    tabsLayout = ifWider smallMonResWidth (toggleLayouts allTabs tallTabs) (toggleLayouts tallTabs allTabs)

    oneBigLayout = renamed [Replace "One Big"] (Mirror (OneBig (2/4) (2/4)))

    -- Other Layout Stuff
    floatWorkSpace      = renamed [Replace "Float"] (borderResize $ addFloatTopBar positionStoreFloat)
    fullBarToggle       = mkToggle (single FULLBAR)
    fullCenterToggle    = mkToggle (single FULLCENTER)
    fullScreenToggle    = mkToggle (single FULL)
    mirrorToggle        = mkToggle (single MIRROR)
    reflectToggle       = mkToggle (single REFLECTX)
    smallMonResWidth    = 2560
    monResWidth         = fromIntegral smallMonResWidth
    showWorkspaceName   = showWName' myShowWNameTheme
    mySpacing           = spacingRaw False (Border gap gap gap gap) True (Border gap gap gap gap) True
    addFloatTopBar      = noFrillsDeco shrinkText topFloatBarTheme

myModMask :: KeyMask
myModMask = mod4Mask -- super (and on my system, hyper) keys

-- Display keyboard mappings using zenity
-- from https://github.com/thomasf/dotfiles-thomasf-xmonad/
--              blob/master/.xmonad/lib/XMonad/Config/A00001.hs
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
    -- added home and end for the two starting workspaces
    -- (On my keyboard they are on the number layer just abouve space)
    wsKeys = ["<Home>", "<End>", "1", "2", "3", "4", "5", "6", "7", "8", "9", "0"]
    dirKeys        = ["j","k","h","l"]
    dirs           = [ D,  U,  L,  R ]

    zipM  m nm ks as f = zipWith (\k d -> (m ++ k, addName nm $ f d)) ks as

    swapMaster' (W.Stack f u d) = W.Stack f [] $ reverse u ++ d

    warpCursor = warpToWindow (1/2) (1/2)

    -- cf https://github.com/pjones/xmonadrc
    --switch :: ProjectTable -> ProjectName -> X ()
    --switch ps name = case Map.lookup name ps of
    --  Just p              -> switchProject p
    --  Nothing | null name -> return ()

    toggleFloat w = windows (\s -> if M.member w (W.floating s)
                    then W.sink w s
                    else W.float w (W.RationalRect (1/3) (1/4) (1/2) (4/5)) s)

    in

    subKeys "System"
    [ ("M-q"                    , addName "Restart XMonad"                  $ spawn "xmonad --restart")
    , ("M-C-q"                  , addName "Rebuild & restart XMonad"        $ spawn "xmonad --recompile && xmonad --restart")
    , ("M-S-q"                  , addName "Quit XMonad"                     $ confirmPrompt hotPromptTheme "Quit XMonad" $ io exitSuccess)
    , ("M-M1-C-S-q"             , addName "Quit XMonad"                     $ confirmPrompt hotPromptTheme "Quit XMonad" $ io exitSuccess)
    , ("M-M1-C-S-x"             , addName "Lock screen"                     $ spawn myLockscreen)
    , ("M-x"                    , addName "notification panel"              $ spawn "toggle notif")
    ] ^++^

    subKeys "Utilities"
    [ ("M-M1-C-S-z"                  , addName "Colour picker"                   $ spawn myColorPicker) -- kill picom before use
    , ("M-M1-C-S-o"                  , addName "On-screen keys"                  $ spawn "killall screenkey || screenkey")
    , ("M-M1-C-S-/"                  , addName "On-screen keys settings"         $ spawn "screenkey --show-settings")
    , ("M-M1-C-S-f"                  , addName "Capture screen"                  $ spawn "screencapt" )
    , ("M-M1-C-S-s"                  , addName "Capture selection"               $ spawn "screencapt area" )
    , ("M-M1-C-S-w"                  , addName "Record screen"                   $ spawn "screencast" )
    , ("M-M1-C-S-r"                  , addName "Record screen - area select"     $ spawn "screencast area" )
    , ("M-;"                         , addName "Warp Cursor"                     $ warpToWindow (1/2) (1/2))
    ] ^++^

    subKeys "Apps"
    [ ("M-<Return>"             , addName "Terminal"                        $ spawn myTerminal)
    , ("M-b"                    , addName "Browser"                         $ spawn myBrowser)
    , ("M-C-b"                  , addName "Work Browser"                    $ bindOn WS [(wsTMP2,    spawn "google-chrome-stable --user-data-dir='/home/oleete/.config/browser/google-chrome-stable'"),
                                                                                         (wsTMP,    spawn "google-chrome-stable --user-data-dir='/home/oleete/.config/browser/google-chrome-stable'"),
                                                                                         (wsWRK,    spawn "google-chrome-stable --user-data-dir='/home/oleete/.config/browser/google-chrome-stable'"),
                                                                                         (wsWRK4,   spawn "google-chrome-stable --user-data-dir='/home/oleete/.config/browser/google-chrome-stable'"),
                                                                                         (wsTHESIS, spawn "google-chrome-stable --user-data-dir='/home/oleete/.config/browser/google-chrome-stable'"),
                                                                                         (wsEXP,    spawn "google-chrome-stable --user-data-dir='/home/oleete/.config/browser/google-chrome-stable'"),
                                                                                         (wsSIM,    spawn "google-chrome-stable --user-data-dir='/home/oleete/.config/browser/google-chrome-stable'"),
                                                                                         ("",       spawn "google-chrome-stable --user-data-dir='/home/oleete/.config/browser/google-chrome-stable-wrk'")])
    , ("M-S-c"                    , addName "Editor"                        $ AL.launchApp myPromptTheme myEditor)
    , ("M-e"                    , addName "Explorer"                        $ bindOn WS [(wsSIM, spawn (myExplorer ++ " -t ~/Projects/JuliaPowderModel ~/UniDrive/1_Thesis/1.4_PowderModel")),
                                                                              (wsEXP, spawn (myExplorer ++ " -t ~/Projects/JuliaPlotting ~/UniDrive/1_Thesis/1.4_PowderModel")),
                                                                              (wsPRO2, spawn (myExplorer ++  " -t ~/Projects/julia-vscode ~/Projects/julia-benchmark-example")),
                                                                              (wsCON, spawn (myExplorer ++  " -t ~/Projects/Configs ~/Projects/ConfigExamples ~/.config")),
                                                                              (wsPER, spawn (myExplorer ++  " -t ~/Downloads ~/PersonalDrive")),
                                                                              (wsWRK, spawn (myExplorer ++  " -t ~/Downloads ~/UniDrive")),
                                                                              ("", spawn (myExplorer ++ " ."))])
    ] ^++^

    subKeys "ScratchPad Apps"
    [ ("M-S-c"                  , addName "NSP Calculator"                  $ namedScratchpadAction scratchpads "calc")
    , ("M-S-<Return>"           , addName "NSP Console"                     $ namedScratchpadAction scratchpads "console")
    , ("M-S-d"                  , addName "NSP Discord"                     $ namedScratchpadAction scratchpads "discord")
    , ("M-S-b"                  , addName "NSP Browser"                     $ bindOn WS [(wsWRK, namedScratchpadAction scratchpads "chromenspwrk"),
                                                                              (wsSIM, namedScratchpadAction scratchpads "chromenspwrk"),
                                                                              (wsEXP, namedScratchpadAction scratchpads "chromenspwrk"),
                                                                              (wsTHESIS, namedScratchpadAction scratchpads "chromenspwrk"),
                                                                              (wsWRK4, namedScratchpadAction scratchpads "chromenspwrk"),
                                                                              ("", namedScratchpadAction scratchpads "chromensp")])
    , ("M-S-t"                  , addName "NSP Tasks"                       $ bindOn WS [(wsWRK, namedScratchpadAction scratchpads "tasksWork"),
                                                                              (wsSIM, namedScratchpadAction scratchpads "tasksWork"),
                                                                              (wsEXP, namedScratchpadAction scratchpads "tasksWork"),
                                                                              (wsTHESIS, namedScratchpadAction scratchpads "tasksWork"),
                                                                              (wsWRK4, namedScratchpadAction scratchpads "tasksWork"),
                                                                              ("", namedScratchpadAction scratchpads "tasks")])
    , ("M-S-n"                  , addName "NSP Keep"                        $ bindOn WS [(wsWRK, namedScratchpadAction scratchpads "keepWrkNsp"),
                                                                              (wsSIM, namedScratchpadAction scratchpads "keepWrkNsp"),
                                                                              (wsEXP, namedScratchpadAction scratchpads "keepWrkNsp"),
                                                                              (wsTHESIS, namedScratchpadAction scratchpads "keepWrkNsp"),
                                                                              (wsWRK4, namedScratchpadAction scratchpads "keepWrkNsp"),
                                                                              ("", namedScratchpadAction scratchpads "keepNsp")])
    , ("M-S-m"                  , addName "NSP Music"                       $ namedScratchpadAction scratchpads "youtubeMusic")
    ] ^++^


    subKeys "Workspaces & Projects"
    [ ("M-a"                       , addName "Launcher"                    $ spawn myLauncher)

    , ("<F8>"                      , addName "prompt select ws"            $ switchProjectPrompt myPromptTheme)
    , ("C-<F8>"                    , addName "prompt select window"        $ windowPrompt myPromptTheme Goto allWindows)
    , ("M-C-s"                     , addName "prompt send to ws"           $ shiftToProjectPrompt myPromptTheme)
    , ("M-C-f"                     , addName "prompt fetch window"         $ windowPrompt myPromptTheme Bring allWindows)

    -- , ("M-w"                       , addName "change project name"         $ renameProjectPrompt myPromptTheme)
    -- , ("M-C-w"                     , addName "change project dir"          $ changeProjectDirPrompt myPromptTheme)
    ] ^++^

    subKeys "Workspaces"
    (
    [ ("M-C-d"                     , addName "Kill other duplicates"       killAllOtherCopies)
    , ("M-d"                       , addName "Duplicate w to all ws"       toggleCopyToAll)
    ]
    ++ zipM "M-"                   "View ws"                               wsKeys [0..] (withNthWorkspace W.greedyView)
    ++ zipM "M-C-"                 "Move w to ws"                          wsKeys [0..] (withNthWorkspace W.shift)
    ++ zipM "M-M1-"                "Copy w to ws"                          wsKeys [0..] (withNthWorkspace copy)
    ) ^++^

    subKeys "Windows"
    (
    [ ("M-<Backspace>"          , addName "Kill"                            kill1)
    , ("M-S-<Backspace>"        , addName "Kill all"                        $ confirmPrompt hotPromptTheme "kill all" killAll)

    , ("M-m"                    , addName "Promote to main"                 $ sequence_ [swapPromote' False, warpCursor])
    , ("M-C-m"                  , addName "Promote to Main"                 $ sequence_ [promote, warpCursor])
    , ("M-M1-m"                 , addName "SubLayout swapMain"              $ onGroup swapMaster')

    , ("M-<Escape>"             , addName "Spawn next window in main"       $ toggleHookNext "Main" >> runLogHook)

    , ("M-<Space>"              , addName "Swap monitor workspaces"         swapNextScreen)
    , ("M-C-<Space>"            , addName "Send window to next monitor"     shiftNextScreen)

    , ("M-n"                    , addName "Focus down"                      $ sequence_ [windows W.focusDown, warpCursor])
    , ("M-p"                    , addName "Focus up"                        $ sequence_ [windows W.focusUp, warpCursor])
    , ("M-<D>"                    , addName "Focus down"                      $ sequence_ [windows W.focusDown, warpCursor])
    , ("M-<U>"                    , addName "Focus up"                        $ sequence_ [windows W.focusUp, warpCursor])
    , ("M-C-n"                  , addName "Shift down"                      $ sequence_ [windows W.swapDown, warpCursor])
    , ("M-C-p"                  , addName "Shift up"                        $ sequence_ [windows W.swapUp, warpCursor])
    , ("M-C-<D>"                  , addName "Shift down"                      $ sequence_ [windows W.swapDown, warpCursor])
    , ("M-C-<U>"                  , addName "Shift up"                        $ sequence_ [windows W.swapUp, warpCursor])

    , ("M-M1-n"                 , addName "SubLayout combine down"          $ withFocused (sendMessage . mergeDir W.focusDown'))
    , ("M-M1-p"                 , addName "SubLayout combine up"            $ withFocused (sendMessage . mergeDir W.focusUp'))
    , ("M-M1-<D>"                 , addName "SubLayout combine down"          $ withFocused (sendMessage . mergeDir W.focusDown'))
    , ("M-M1-<U>"                 , addName "SubLayout combine up"            $ withFocused (sendMessage . mergeDir W.focusUp'))

    , ("M-<R>"                  , addName "Cycle up"                        rotSlavesUp)
    , ("M-<L>"                  , addName "Cycle down"                      rotSlavesDown)
    , ("M-C-<R>"                , addName "Cycle up"                        rotAllUp)
    , ("M-C-<L>"                , addName "Cycle down"                      rotAllDown)


    , ("M-f"                    , addName "Fullscreen"                      $ sequence_ [ withFocused $ windows . W.sink
                                                                            , sendMessage $ XMonad.Layout.MultiToggle.Toggle FULL ])

    , ("M-w"                    , addName "Maximize"                        $ sequence_ [ withFocused $ windows . W.sink
                                                                            , sendMessage $ XMonad.Layout.MultiToggle.Toggle FULLBAR ])

    , ("M-c"                  , addName "Center Focus"                      $ sequence_ [ withFocused $ windows . W.sink
                                                                            , sendMessage $ XMonad.Layout.MultiToggle.Toggle FULLCENTER
                                                                            , P.sendKey P.noModMask xK_F11])

    , ("M-h"                    , addName "Navigate Left"                   $ sequence_ [windowGo L True, warpCursor])
    , ("M-j"                    , addName "Navigate Down"                   $ sequence_ [windowGo D True, warpCursor])
    , ("M-k"                    , addName "Navigate Up"                     $ sequence_ [windowGo U True, warpCursor])
    , ("M-l"                    , addName "Navigate Right"                  $ sequence_ [windowGo R True, warpCursor])

    , ("M-C-h"                  , addName "Move Left"                       $ sequence_ [windowSwap L True, warpCursor])
    , ("M-C-j"                  , addName "Move Down"                       $ sequence_ [windowSwap D True, warpCursor])
    , ("M-C-k"                  , addName "Move Up"                         $ sequence_ [windowSwap U True, warpCursor])
    , ("M-C-l"                  , addName "Move Right"                      $ sequence_ [windowSwap R True, warpCursor])
    ]

    -- ++ zipM' "M-"               "Navigate window"                           dirKeys dirs windowGo True
    -- ++ zipM' "M-C-"             "Move window"                               dirKeys dirs windowSwap True
    ++ zipM  "M-M1-"            "Merge w/sublayout"                         dirKeys dirs (sendMessage . pullGroup)

    ) ^++^

    subKeys "Layout Management"
    [ ("M-<Tab>"                , addName "Cycle all layouts"               $ sendMessage NextLayout)
    , ("M-S-<Tab>"              , addName "Reset layout"                    $ setLayout $ XMonad.layoutHook conf)
    , ("M-M1-<Tab>"             , addName "Cycle sublayout"                 $ toSubl NextLayout)
    , ("M-C-<Tab>"              , addName "Toggle sublayout"                $ bindOn LD [("Notebook", sendMessage ToggleMiddle)
                                                                                        ,("Three Col", sendMessage ToggleMid)
                                                                                        ,("", sendMessage ToggleLayout)])

    , ("M-y"                    , addName "Float tiled w"                   $ withFocused toggleFloat)
    , ("M-C-y"                  , addName "Tile all floating w"             sinkAll)

    , ("M-,"                    , addName "Decrease main windows"           $ sendMessage (IncMasterN (-1)))
    , ("M-."                    , addName "Increase main windows"           $ sendMessage (IncMasterN 1))
    , ("M-C-,"                  , addName "Decrease main windows"           $ sendMessage (IncColumnN (-1)))
    , ("M-C-."                  , addName "Increase main windows"           $ sendMessage (IncColumnN 1))
    , ("M-M1-."                 , addName "toSubl IncMainN 1"               $ toSubl $ IncMasterN 1)
    , ("M-M1-,"                 , addName "toSubl IncMainN -1"              $ toSubl $ IncMasterN (-1))

    , ("M-["                    , addName "Shrink Main"                     $ sendMessage Shrink)
    , ("M-]"                    , addName "Expand Main"                     $ sendMessage Expand)
    , ("M-C-["                  , addName "Shrink height"                   $ sendMessage MirrorShrink)
    , ("M-C-]"                  , addName "Expand height"                   $ sendMessage MirrorExpand)

    , ("M-r"                    , addName "Reflect/Rotate"                  $ bindOn LD [("Notebook",sendMessage ToggleSide)
                                                                                        ,("", sendMessage (XMonad.Layout.MultiToggle.Toggle REFLECTX))])
    , ("M-C-r"                  , addName "Reflect Stack"                   $ sendMessage ToggleStackDir)

    , ("M-g"                    , addName "Un-merge from sublayout"         $ withFocused (sendMessage . UnMerge))
    , ("M-M1-g"                 , addName "Unmerge all from sublayout"      $ withFocused (sendMessage . UnMergeAll))
    , ("M-M1-S-g"               , addName "Merge all into sublayout"        $ withFocused (sendMessage . MergeAll))
    ]
    where
      toggleCopyToAll = wsContainingCopies >>= \case
              [] -> windows copyToAll
              _ -> killAllOtherCopies

--    subKeys "Media Controls"
--    [
--    ("<XF86AudioMicMute>"      , addName "Mic Mute"                    $ spawn "notify-send mic mute")
--    ]


-- Mouse bindings: default actions bound to mouse events
-- Includes window snapping on move/resize using X.A.FloatSnap
-- I got rid of that, seemed to be causing a lot of lag
-- Includes window w/h ratio constraint (square) using X.H.ConstrainedResize
-- Think I got rid of this as well, I don't use floats much at all
myMouseBindings :: XConfig l -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings XConfig {XMonad.modMask = myModMask} = M.fromList
    [ ((myModMask,               button1) ,\w -> focus w
      >> mouseMoveWindow w
      >> windows W.shiftMaster)

    , ((myModMask .|. shiftMask, button1), \w -> focus w
      >> mouseMoveWindow w
      >> windows W.shiftMaster)

    , ((myModMask,               button3), \w -> focus w
      >> mouseResizeWindow w
      >> windows W.shiftMaster)

    , ((myModMask .|. shiftMask, button3), \w -> focus w
      >> Sqr.mouseResizeWindow w True
      >> windows W.shiftMaster)
    ]

----------------------------------------------------------------------------------------------------
-- Startup                                                                                        --
----------------------------------------------------------------------------------------------------

myStartupHook :: X ()
myStartupHook = do
    spawnOnce myCompositor
    spawn myWallpaper
    spawnOnce myTray
    -- spawnOnce "logid -c ~/.config/logid/logid.cfg"
    spawnOnce "insync start; insync hide"
    spawnOnce "xsetroot -cursor_name left_ptr"
    spawnOnce "deadd-notification-center &"


----------------------------------------------------------------------------------------------------
-- Log                                                                                            --
----------------------------------------------------------------------------------------------------

myLogHook :: Handle -> X ()
myLogHook h = do

    -- following block for copy windows marking
    copies <- wsContainingCopies
    let check ws | ws `elem` copies =
                   pad . xmobarColor base2 "" . wrap "" ""  $ ws
                 | otherwise = pad . xmobarColor base2 "" . wrap "" "" $ ws

    -- updatePointer (0.5, 0.5) (0, 0)
    fadeWindowsLogHook myFadeHook
    ewmhDesktopsLogHook
    masterHistoryHook
    dynamicLogWithPP . filterOutWsPP ["NSP", wsTMP, wsTMP2] $ def

        { ppCurrent             = xmobarColor yellow "" . wrap "[" "]" . clickable
        , ppTitle               = xmobarColor yellow "" . wrap "<action=xdotool key Super+w>" "</action>" . shorten 40
        , ppVisible             = xmobarColor yellow  "" . clickable
        , ppUrgent              = xmobarColor red    "" . wrap "!" "!"
        , ppHidden              = check . clickable
        , ppHiddenNoWindows     = const ""
        , ppSep                 = "  :  "
        , ppWsSep               = " "
        , ppLayout              = xmobarColor green "" . wrap "<action=xdotool key Super+Tab>" "</action>"
        , ppOrder               = id
        , ppOutput              = hPutStrLn h
        , ppSort                = ppSort def
        , ppExtras              = [willHookNextPP "Main" $ xmobarColor red "> Place in Main <fn=1></fn"] }


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
-- Actions                                                                                        --
----------------------------------------------------------------------------------------------------


----------------------------------------------------------------------------------------------------
-- Urgency Hook                                                                                   --
----------------------------------------------------------------------------------------------------
-- from https://pbrisbin.com/posts/using_notify_osd_for_xmonad_notifications/
data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
    urgencyHook LibNotifyUrgencyHook w = do
        wsname     <- getName w
        Just idx <- W.findTag w <$> gets windowset

        safeSpawn "notify-send" [show wsname, "workspace " ++ idx]
-- cf https://github.com/pjones/xmonadrc


----------------------------------------------------------------------------------------------------
-- New Window Actions                                                                             --
----------------------------------------------------------------------------------------------------

-- https://wiki.haskell.org/Xmonad/General_xmonad.hs_config_tips#ManageHook_examples
-- <+> manageHook defaultConfig

myManageHook :: ManageHook
myManageHook =
        manageSpecific
    <+> manageDocks
    <+> toggleHook' "Main" doMain tileBelow
    <+> namedScratchpadManageHook scratchpads
    -- <+> fullscreenManageHook
    <+> manageSpawn
    where
        manageSpecific = composeOne
            [ resource =? "desktop_window" -?> doIgnore
            , resource =? "stalonetray"    -?> doIgnore
            -- , resource =? "vlc"    -?> doFloat
            -- , resource =? "Steam"    -?> doFloat
            , resource =? "gnome-calculator" -?> doCenterFloat
            , resource =? "pavucontrol" -?> doRectFloat (W.RationalRect ((3840-500-6)/3840) ((2160-700-42)/2160) (500/3840) (700/2160))
            , resource =? "nm-connection-editor" -?> doRectFloat (W.RationalRect ((3840-500-6)/3840) ((2160-700-42)/2160) (500/3840) (700/2160))
            , resource =? "galendae" -?> doRectFloat (W.RationalRect ((3840-300-6)/3840) ((2160-300-42)/2160) (300/3840) (300/2160))
            , resource =? "nitrogen" -?> doCenterFloat
            , resource =? "Tasks" -?> doRectFloat (W.RationalRect (1 / 4) (1 / 4) (1 / 2) (1 / 2))
            , resource =? "WrkTasks" -?> doRectFloat (W.RationalRect (1 / 4) (1 / 4) (1 / 2) (1 / 2))
            , className =? "Keep" -?> doRectFloat (W.RationalRect (1 / 4) (1 / 4) (1 / 2) (1 / 2))
            , className =? "WrkKeep" -?> doRectFloat (W.RationalRect (1 / 4) (1 / 4) (1 / 2) (1 / 2))
            , resource =? "kittyconsole" -?> doRectFloat (W.RationalRect (3 / 5) (3 / 5) (1 / 3) (1 / 3))
            , className =? "Chromensp" -?> doRectFloat (W.RationalRect (1 / 4) (1 / 8) (1 / 2) (3 / 4))
            , className =? "Chromewrknsp" -?> doRectFloat (W.RationalRect (1 / 4) (1 / 8) (1 / 2) (3 / 4))
            , resource =? youtubeMusicResource -?> doRectFloat (W.RationalRect (1 / 4) (1 / 4) (1 / 2) (1 / 2))
            , resource =? discordResource -?> doRectFloat (W.RationalRect (1 / 4) (1 / 4) (1 / 2) (1 / 2))
            , title =? "Picture-in-picture" -?> doRectFloat (W.RationalRect (2/3) (1/4) (1/4) (1/4))
            , transience
            , isBrowserDialog -?> forceCenterFloat
            -- , isConsole -?> forceCenterFloat
            , isRole =? gtkFile  -?> forceCenterFloat
            , isDialog -?> doCenterFloat
            , isRole =? "pop-up" -?> doCenterFloat
            , isInProperty "_NET_WM_WINDOW_TYPE"
                           "_NET_WM_WINDOW_TYPE_SPLASH" -?> doCenterFloat]
            -- , resource =? "console" -?> tileBelowNoFocus]
            -- , isFullscreen -?> doFullFloat
            -- , pure True -?> tileBelow ]
        isBrowserDialog = isDialog <&&> className =? myBrowserClass
        gtkFile = "GtkFileChooserDialog"
        isRole = stringProperty "WM_WINDOW_ROLE"
        -- insert WHERE and focus WHAT
        doMain = insertPosition Master Newer
        tileBelow = insertPosition End Newer

----------------------------------------------------------------------------------------------------
-- X Event Actions                                                                                --
----------------------------------------------------------------------------------------------------

-- for reference, the following line is the same as dynamicTitle myDynHook
-- <+> dynamicPropertyChange "WM_NAME" myDynHook

-- I'm not really into full screens without my say so... I often like to
-- fullscreen a window but keep it constrained to a window rect (e.g.
-- for videos, etc. without the UI chrome cluttering things up). I can
-- always do that and then full screen the subsequent window if I want.
-- THUS, to cut a long comment short, no fullscreenEventHook

myHandleEventHook :: Event -> X All
myHandleEventHook = docksEventHook
                <+> fadeWindowsEventHook
                <+> dynamicTitle myDynHook
                <+> handleEventHook def
                <+> XMonad.Util.Hacks.windowedFullscreenFixEventHook
                -- <+> XMonad.Layout.Fullscreen.fullscreenEventHook
                -- <+> XMonad.Hooks.EwmhDesktops.fullscreenEventHook
    where
        myDynHook = composeAll
            [ isDiscord --> forceCenterFloat
            ]

----------------------------------------------------------------------------------------------------
-- Custom hook helpers                                                                            --
----------------------------------------------------------------------------------------------------

-- from:
-- https://github.com/pjones/xmonadrc/blob/master/src/XMonad/Local/Action.hs
--
-- Useful when a floating window requests stupid dimensions.  There
-- was a bug in Handbrake that would pop up the file dialog with
-- almost no height due to one of my rotated monitors.

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
