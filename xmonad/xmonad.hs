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
import XMonad.Layout.ShowWName
import XMonad.Layout.SimpleFocus
import XMonad.Layout.Spacing

import XMonad.Util.ClickableWorkspaces
import XMonad.Util.EZConfig
import XMonad.Util.Hacks
import XMonad.Util.NamedScratchpad
import XMonad.Util.Paste as P
import XMonad.Util.SpawnOnce

import XMonad.Prompt.ConfirmPrompt (confirmPrompt)
import XMonad.Actions.WithAll (killAll)
import XMonad.Prompt
import XMonad.Hooks.RefocusLast
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
wsWRK    = "Wrk"
wsWRK1   = "Wrk1"
wsSIM    = "Sim"
wsTHESIS = "Thesis"
wsEXP    = "Exp"
wsANSYS  = "ANSYS"

myWorkspaces :: [[Char]]
myWorkspaces = [wsTMP, wsTMP2, wsPER1, ws3D, wsDND, wsCON, wsPER, wsWRK, wsEXP, wsSIM, wsTHESIS, wsWRK1, wsQMK, wsANSYS]

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
                , projectDirectory  = "~/Projects/Thesis"
                , projectStartHook  = Just $ do spawnOn wsTHESIS myTerminal
                                                spawnOn wsTHESIS ("sleep .2; " ++ myBrowser)
                }
    , Project   { projectName       = wsANSYS
                , projectDirectory  = "~/Projects/ANSYSpowderModel"
                , projectStartHook  = Just $ do spawnOn wsEXP myTerminal
                                                spawnOn wsEXP ("sleep .2; " ++ myBrowser)
                }
    ]

----------------------------------------------------------------------------------------------------
-- Applications                                                                                   --
----------------------------------------------------------------------------------------------------
myTerminal     = "/home/oleete/.config/bin/kittyMaker"
myTerminalRemote = "/home/oleete/.config/bin/kittyRemote"
myBrowser      = "/home/oleete/.config/bin/browser"
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
            $ showWName' myShowWNameTheme
            $ mkToggle (single FULL)
            $ spacingRaw False (Border gap gap gap gap) True (Border gap gap gap gap) True
            $ mkToggle (single FULLBAR)
            $ mkToggle (single FULLCENTER)
              notebookLayout
    where
    notebookMulti   = Notebook 1000  True True True 1 2 reSize 2 (2/3)
    notebookThesis  = Notebook 1000  True True True 1 3 reSize 2 (2/3)
    notebookColumns = Notebook 1000 False True True 4 4 reSize 2 (2/3)
    notebookLaptop = Notebook 1000 True False False 1 2 reSize 2 (2/3)
    notebookLayout = onWorkspaces [wsTMP, wsTMP2, wsPER, wsWRK] notebookColumns
                   $ ifWider 1920 (onWorkspaces [wsTHESIS, ws3D] notebookThesis notebookMulti) notebookLaptop

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

    , ("M-f M-f"            , spawn "/home/oleete/.config/bin/rofiScript")
    , ("M-f f"              , spawn "/home/oleete/.config/bin/rofiScript")
    , ("M-f M-w"            , spawn "/home/oleete/.config/bin/wsHarpoon menu")
    , ("M-f w"              , spawn "/home/oleete/.config/bin/wsHarpoon menu")
    , ("M-f M-g"            , spawn "/home/oleete/.config/bin/wsHarpoon moveMenu")
    , ("M-f g"              , spawn "/home/oleete/.config/bin/wsHarpoon moveMenu")
    , ("M-f M-p"            , spawn "rofi -matching fuzzy -show drun -show-icons")
    , ("M-f p"              , spawn "rofi -matching fuzzy -show drun -show-icons")

    , ("M-p"                      , spawn "rofi -matching fuzzy -show drun -show-icons")
    , ("<XF86MonBrightnessDown>"  , spawn "/home/oleete/.config/bin/brightness -dec 5")
    , ("<XF86MonBrightnessUp>"    , spawn "/home/oleete/.config/bin/brightness -inc 5")
    , ("<XF86AudioLowerVolume>"   , spawn "/home/oleete/.config/bin/volume set-sink-volume @DEFAULT_SINK@ -5%" )
    , ("<XF86AudioRaiseVolume>"   , spawn "/home/oleete/.config/bin/volume set-sink-volume @DEFAULT_SINK@ +5%" )
    , ("<XF86AudioMute>"          , spawn "/home/oleete/.config/bin/volume set-sink-volume @DEFAULT_SINK@ 0%" )
    , ("<XF86Display>"            , spawn "/home/oleete/.config/bin/displayctl" )
    , ("<XF86AudioPlay>"          , spawn "playerctl play")
    , ("<XF86AudioStop>"          , spawn "playerctl stop")
    , ("<XF86AudioPause>"         , spawn "playerctl pause")
    , ("<XF86AudioPrev>"          , spawn "playerctl previous")
    , ("<XF86AudioNext>"          , spawn "playerctl next")
    , ("<Print>"                  , spawn "/home/oleete/.config/bin/screencapt area")
    , ("M-<Esc>"                  , upPointer $ allNamedScratchpadAction scratchpads "sysMon")
    , ("M-S-<Esc>"                , upPointer $ allNamedScratchpadAction scratchpads "sysMon")

    , ("M-<Return>"         , klBind " 'kitty @ launch'" (upPointer $ runOrRaise myTerminal (className =? "kitty")))

    , ("M-n"                , rnklBind (spawn (myTerminalRemote ++ " 'kitty @ launch'")) " focusEditor" (upPointer $ sequence_ [raise (className =? "kitty"), spawn (myTerminalRemote ++ " focusEditor")]))
    , ("M-M1-n"             , upPointer $ spawn myTerminal)

    , ("M-i"                , clBind (P.sendKey controlMask xK_t) (upPointer $ runOrRaise myBrowser (className =? "Google-chrome")))
    , ("M-M1-i"             , upPointer $ spawn myBrowser)

    , ("M-e"                , upPointer $ runOrRaise "zathura" (className =? "Zathura"))

    , ("M-S-c"              , upPointer $ allNamedScratchpadAction scratchpads "calc")
    , ("M-S-<Return>"       , upPointer $ allNamedScratchpadAction scratchpads "console")
    , ("M-S-d"              , upPointer $ allNamedScratchpadAction scratchpads "discord")
    , ("M-S-m"              , upPointer $ allNamedScratchpadAction scratchpads "youtubeMusic")
    , ("M-S-t"              , upPointer $ wrkNSP "tasksWork" "tasks")
    , ("M-S-n"              , upPointer $ wrkNSP "keepWrkNsp" "keepNsp")

    , ("M-<Backspace>"      , nkclBind "DeleteBuffer" (P.sendKey (controlMask .|. shiftMask) xK_BackSpace) (P.sendKey controlMask xK_w) kill)
    , ("M-M1-<Backspace>"   , kill)
    , ("M-S-<Backspace>"    , confirmPrompt hotPromptTheme "kill all" killAll)

    , ("M-<Left>"           , kcBind (P.sendKey (controlMask .|. shiftMask) xK_Left) (P.sendKey (controlMask .|. shiftMask) xK_Tab))
    , ("M-<Right>"          , kcBind (P.sendKey (controlMask .|. shiftMask) xK_Right) (P.sendKey controlMask xK_Tab))
    , ("M-<Down>"           , windows W.focusDown)
    , ("M-<Up>"             , windows W.focusUp)


    , ("M-c M-f"            , klBind " kittyFullscreen" (P.sendKey noModMask xK_F11))
    , ("M-c f"              , klBind " kittyFullscreen" (P.sendKey noModMask xK_F11))
    , ("M-c M-w"            , toggleLayout FULL)
    , ("M-c w"              , toggleLayout FULL)
    , ("M-c M-s"            , toggleLayout FULLBAR)
    , ("M-c s"              , toggleLayout FULLBAR)
    , ("M-c M-c"            , toggleLayout FULLCENTER)
    , ("M-c c"              , toggleLayout FULLCENTER)
    , ("M-c M-x"            , nklBind "ZenMode" " kittyFullscreen" (toggleLayout FULLCENTER))
    , ("M-c x"              , nklBind "ZenMode" " kittyFullscreen" (toggleLayout FULLCENTER))

    , ("M-h"                , nklBind "KittyNavigateleft"   " moveWindow left h" (upPointer $ windowGo L True))
    , ("M-j"                , nklBind "KittyNavigatebottom" " moveWindow bottom j" (upPointer $ windowGo D True))
    , ("M-k"                , nklBind "KittyNavigatetop"    " moveWindow top k" (upPointer $ windowGo U True))
    , ("M-l"                , nklBind "KittyNavigateright"  " moveWindow right l" (upPointer $ windowGo R True))
    , ("M-M1-S-h"           , upPointer $ windowGo L True)
    , ("M-M1-S-j"           , upPointer $ windowGo D True)
    , ("M-M1-S-k"           , upPointer $ windowGo U True)
    , ("M-M1-S-l"           , upPointer $ windowGo R True)
    , ("M-M1-h"             , upPointer $ windowSwap L True)
    , ("M-M1-j"             , upPointer $ windowSwap D True)
    , ("M-M1-k"             , upPointer $ windowSwap U True)
    , ("M-M1-l"             , upPointer $ windowSwap R True)

    , ("M-m"                , klBind " mainMove"   (upPointer $ swapPromote' False))
    , ("M-M1-m"             , upPointer $ swapPromote' False)
    , ("M-M1-S-m"           , upPointer $ swapPromote' False)
    , ("M-v"                , myFocusMaster)

    , ("M-y"                , upPointer $ withFocused toggleFloat)
    , ("M-M1-y"             , upFocus sinkAll)

    , ("M-,"                , sendMessage (IncMasterN (-1)))
    , ("M-."                , sendMessage (IncMasterN 1))
    , ("M-M1-,"             , sendMessage (IncColumnN (-1)))
    , ("M-M1-."             , sendMessage (IncColumnN 1))
    , ("M-["                , sendMessage Shrink)
    , ("M-]"                , sendMessage Expand)
    , ("M-M1-["             , sendMessage MirrorShrink)
    , ("M-M1-]"             , sendMessage MirrorExpand)

    , ("M-g"                , upFocus $ sendMessage ToggleSide)
    , ("M-M1-g"             , upFocus $ sendMessage ToggleStackDir)
    , ("M-S-g"              , upFocus $ sendMessage ToggleMiddle)

    , ("M-a"                , spawn "/home/oleete/.config/bin/wsHarpoon jump 1")
    , ("M-r"                , spawn "/home/oleete/.config/bin/wsHarpoon jump 2")
    , ("M-s"                , spawn "/home/oleete/.config/bin/wsHarpoon jump 3")
    , ("M-t"                , spawn "/home/oleete/.config/bin/wsHarpoon jump 4")
    , ("M-M1-a"             , spawn "/home/oleete/.config/bin/wsHarpoon move 1")
    , ("M-M1-r"             , spawn "/home/oleete/.config/bin/wsHarpoon move 2")
    , ("M-M1-s"             , spawn "/home/oleete/.config/bin/wsHarpoon move 3")
    , ("M-M1-t"             , spawn "/home/oleete/.config/bin/wsHarpoon move 4")

    , ("M-w M-w"            , spawn "/home/oleete/.config/bin/wsHarpoon add")
    , ("M-w w"              , spawn "/home/oleete/.config/bin/wsHarpoon add")
    , ("M-w M-e"            , spawn "/home/oleete/.config/bin/wsHarpoon modify")
    , ("M-w e"              , spawn "/home/oleete/.config/bin/wsHarpoon modify")
    , ("M-w M-p"            , spawn "/home/oleete/.config/bin/wsHarpoon makePreset")
    , ("M-w p"              , spawn "/home/oleete/.config/bin/wsHarpoon makePreset")

    , ("M-o"                , upPointer toggleFocus)
    , ("M-M1-o"             , upPointer swapWithLast)
    , ("M-<Space>"          , upFocus $ toggleWS' ["NSP"])
    , ("M-M1-<Space>"       , upFocus $ shiftToggleWS' ["NSP"])
    ]
    ++ zipM "M-"            wsKeys [0..] (withNthWorkspace W.greedyView)
    ++ zipM "M-M1-"         wsKeys [0..] (withNthWorkspace W.shift)
    where
        upFocus a = sequence_ [a, focusUnderPointer]
        upPointer a = sequence_ [a, updatePointer (0.5, 0.5) (0.25, 0.25)]

        wsKeys = ["S-4", "S-7", "1", "2", "3", "4", "5", "6", "7", "8", "9", "0"]
        zipM  m ks as f = zipWith (\k d -> (m ++ k, upFocus $ f d)) ks as

        toggleFloat w = windows (\s -> if M.member w (W.floating s)
                            then W.sink w s
                            else W.float w (W.RationalRect (1/4) (1/4) (1/2) (1/2)) s)

        wrkNSP work personal = bindOn C.WS [(wsWRK, allNamedScratchpadAction scratchpads work)
                                         ,(wsSIM, allNamedScratchpadAction scratchpads work)
                                         ,(wsEXP, allNamedScratchpadAction scratchpads work)
                                         ,(wsTHESIS, allNamedScratchpadAction scratchpads work)
                                         ,("", allNamedScratchpadAction scratchpads personal)]

        nklBind nvr kitty leftover = bindFirst [(title =? "MainEditor", spawn ("/home/oleete/.config/bin/nvrWS " ++ nvr))
                                               ,(className =? "kitty", spawn (myTerminalRemote ++ kitty))
                                               ,(pure True, leftover)]

        rnklBind nvr kitty leftover = bindFirst [(title =? "MainEditor", nvr)
                                                ,(className =? "kitty", spawn (myTerminalRemote ++ kitty))
                                                ,(pure True, leftover)]

        klBind kitty leftover = bindFirst [(className =? "kitty", spawn (myTerminalRemote ++ kitty))
                                          ,(pure True, leftover)]

        clBind chrome leftover = bindFirst [(className =? "Google-chrome", chrome)
                                           ,(pure True, leftover)]

        kcBind kitty chrome = bindFirst [(className =? "kitty", kitty)
                                        ,(className =? "Google-chrome", chrome)]

        nkclBind nvr kitty chrome leftover = bindFirst [(title =? "MainEditor", spawn ("/home/oleete/.config/bin/nvrWS " ++ nvr))
                                                       ,(className =? "kitty", kitty)
                                                       ,(className =? "Google-chrome", chrome)
                                                       ,(pure True, leftover)]

        toggleLayout layout = sequence_ [ withFocused $ windows . W.sink, sendMessage $ XMonad.Layout.MultiToggle.Toggle layout, focusUnderPointer ]

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
    spawnOnce "xsetroot -cursor_name left_ptr"
    spawnOnce "picom -b --config ~/.config/picom/picom.conf"
    spawnOnce "insync start; insync hide"
    spawnOnce "/home/oleete/.config/bin/startupScript"
    spawnOnce "/home/oleete/.config/bin/connect_screen.py"

----------------------------------------------------------------------------------------------------
-- Log                                                                                            --
----------------------------------------------------------------------------------------------------

mySB0 = statusBarPropTo "_XMONAD_LOG_0" "xmobar -x 0 ~/.config/xmobar/xmobar0.conf" (clickablePP $ filterOutWsPP ["NSP"] myPP)
mySB1 = statusBarPropTo "_XMONAD_LOG_1" "xmobar -x 1 ~/.config/xmobar/xmobar1.conf" (clickablePP $ filterOutWsPP ["NSP"] myPP)
mySB2 = statusBarPropTo "_XMONAD_LOG_2" "xmobar -x 2 ~/.config/xmobar/xmobar2.conf" (clickablePP $ filterOutWsPP ["NSP"] myPP)
mySB3 = statusBarPropTo "_XMONAD_LOG_3" "xmobar -x 3 ~/.config/xmobar/xmobar3.conf" (clickablePP $ filterOutWsPP ["NSP"] myPP)

myPP = def
    { ppCurrent = xmobarColor active "" . wrap ("<box type=Bottom width=2 mt=2 color=" ++ active ++ ">") "</box>"
    , ppVisible = xmobarColor active ""
    , ppHidden  = xmobarColor dull  ""
    , ppTitle   = xmobarColor foreground "" . wrap ("<box type=Bottom width=2 mt=2 color=" ++ yellow ++ "><fc=" ++ yellow ++ ">") "</fc></box>" . shorten 30
    , ppLayout  = const ""
    , ppSep = xmobarColor foreground "" " | "
    , ppOrder = reverse
    }

barSpawner :: ScreenId -> IO StatusBarConfig
barSpawner 0 = pure mySB0
barSpawner 1 = pure mySB1
barSpawner 2 = pure mySB2
barSpawner _ = pure mySB3

myLogHook = do
    masterHistoryHook
    -- nsHideOnFocusLoss scratchpads
    refocusLastLogHook 

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
            , resource =? "wsHarpoon" -?> doRectFloat (W.RationalRect (3 / 10) (3 / 10) (2 / 5) (2 / 5))
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
myHandleEventHook = handleEventHook def
                <+> XMonad.Util.Hacks.windowedFullscreenFixEventHook
