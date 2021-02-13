import XMonad
import System.Exit
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Place
import XMonad.Layout.Fullscreen ( fullscreenManageHook, fullscreenSupport )
import XMonad.Layout.LimitWindows
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Layout.Spacing
import XMonad.Layout.ResizableTile
import XMonad.Layout.Renamed
import XMonad.Layout.SimplestFloat
import XMonad.Layout.PerWorkspace
import XMonad.Util.Run(spawnPipe, hPutStrLn)
import XMonad.Util.SpawnOnce
import XMonad.Util.EZConfig
import XMonad.Wallpaper
import XMonad.Util.NamedScratchpad
import XMonad.Actions.CopyWindow
import XMonad.Actions.WithAll
import XMonad.Actions.Search
import XMonad.Actions.Promote
import XMonad.Actions.CycleWS
import XMonad.Actions.CycleRecentWS
import XMonad.Actions.CycleWindows
import qualified XMonad.Actions.FlexibleResize as Flex
import qualified XMonad.Actions.TreeSelect as TS
import XMonad.Prompt
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.ConfirmPrompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Window
import qualified  XMonad.StackSet as W

import Data.Monoid
import Data.Tree
import qualified Data.Map as M


main = do
    spawn "${HOME}/.bin/picom --experimental-backends -b &"
    xmproc <- spawnPipe "xmobar ~/.xmobarrc"
    setRandomWallpaper [ "$HOME/Wallpapers" ]
    xmonad $fullscreenSupport $docks $ewmh desktopConfig
      { terminal   =      myTerminal,
        borderWidth =     myBorderWidth,
        focusFollowsMouse = myFocusFollowsMouse,
        clickJustFocuses = myClickJustFocuses,
        layoutHook =      myLayoutHook,
        manageHook =      myManageHook,
        handleEventHook = myHandleEventHook,
        logHook =         myLogHook xmproc,
        modMask    =      myModMask,
        workspaces =      myWorkspaces,
        startupHook        = myStartupHook
      } `additionalKeysP` myAdditionalKeysP
        `additionalMouseBindings` myAdditionalMouseBindings

--Bind Mod to the Windows Key
myModMask = mod4Mask
myWorkspaces = [ "Main", "Web", "Files", "Dev", "Col", "Ent", "7", "8", "Back" ]
myBorderWidth = 1
myTerminal = "alacritty"
myFocusFollowsMouse = True
myClickJustFocuses = False

myLayoutHook =  onWorkspace "Main"
                ( tallLayout
                  |||fullLayout 
                  ||| mirrorTallLayout 
                )
              $ onWorkspace "Web" 
                ( fullLayout
                  ||| tallLayout 
                )
              $ onWorkspace "Col" floatingLayout
              $ tallLayout ||| fullLayout
 
tallLayout = renamed [Replace "Tall"]
       $ limitWindows 3
       $ spacingRaw True (Border 0 5 0 5) True (Border 5 0 5 0) True 
       $ smartBorders
       $ avoidStruts
       $ ResizableTall 1 (3/100) (1/2) []
fullLayout = renamed [ Replace "Maximized"]
       $ spacingRaw True (Border 0 5 0 5) True (Border 5 0 5 0) True 
       $ noBorders
       $ Full
mirrorTallLayout = renamed [Replace "Mirror Tall"]
       $ spacingRaw True (Border 0 5 0 5) True (Border 5 0 5 0) True 
       $ smartBorders
       $ avoidStruts 
       $ Mirror(ResizableTall 1 (3/100) (1/2) [])
floatingLayout = simplestFloat       

myLogHook h = dynamicLogWithPP  xmobarPP {
    ppOutput  =        hPutStrLn h ,
    ppTitle   =        xmobarColor "lightgreen" "" . shorten 40,
    ppCurrent =        xmobarColor "yellow"     "" . shorten 10,
    ppHidden  =        xmobarColor "grey"       "" . shorten 10,
    ppUrgent  =        xmobarColor "red"        "" . shorten 10
  }

myScratchPads = [
    NS "dropdownTerminal" spawnTerm findTerm manageTerm 
 ]
   where
     spawnTerm = myTerminal ++ " --class dropdownTerminal" 
     findTerm =   resource =? "dropdownTerminal"
     manageTerm = customFloating $ W.RationalRect x y w h
                      where
                        x = 0.25
                        y = 0.25
                        w = 0.50
                        h = 0.50

myManageHook = composeOne
  [
    transience      --move transient windows to their parent
  , isDialog        -?> doCenterFloat
  , isFullscreen    -?> doFullFloat
  ] <+> composeAll
  [
    manageDocks,
    floatingWindowsHook,
    fullscreenManageHook,
    namedScratchpadManageHook myScratchPads,
    placeHook simpleSmart,
    manageHook defaultConfig
 ] 

floatingWindowsHook = composeAll [
    className =? "Gimp"           --> doFloat,
    className =? "zoom"           --> doFloat,
    className =? "TeamViewer"     --> doFloat,
    className =? "mpv"            --> (doRectFloat $ W.RationalRect 0.80 0.80 0.20 0.20),
    className =? "dropdownTerminal" --> (doRectFloat $ W.RationalRect 0.25 0.25 0.25 0.25)
 ]

myHandleEventHook = fullscreenEventHook

-- State variable init
brightnessVal = 0.6

myStartupHook = return()

--  Customize the way 'XMonad.Prompt' looks and behaves.
--  It's a great replacement for dzen.
myXPConfig = def
  { position          = Top
  , alwaysHighlight   = True
  , promptBorderWidth = 0
  , font              = "xft:monospace:size=9"
  , fgColor           = "yellow"
  , searchPredicate   = fuzzyMatch
  , sorter            = fuzzySort
  }

treeselectAction a = TS.treeselectAction a
   [ Node (TS.TSNode "+ Audio" "Adjust system audio" (return ())) 
      [ Node (TS.TSNode "Toggle Mute" "Toogle Mute" (spawn toggleMuteCmd)) []
      , Node (TS.TSNode "90%" "90% Volume" (volume 90)) []
      , Node (TS.TSNode "80%" "80% Volume" (volume 80)) []
      , Node (TS.TSNode "70%" "70% Volume" (volume 70)) []
      , Node (TS.TSNode "60%" "60% Volume" (volume 60)) []
      , Node (TS.TSNode "50%" "50% Volume" (volume 50)) []
      , Node (TS.TSNode "40%" "40% Volume" (volume 40)) []
      , Node (TS.TSNode "30%" "30% Volume" (volume 30)) []
      , Node (TS.TSNode "20%" "20% Volume" (volume 20)) []
      ]
   , Node (TS.TSNode "+ Brightness" "Adjust system brightness" (return ())) 
      [ Node (TS.TSNode "Full" "Full Brightness" (brightness 0.99)) []
      , Node (TS.TSNode "45%" "45% Brightness" (brightness 0.45)) []
      , Node (TS.TSNode "50%" "50% Brightness" (brightness 0.50)) []
      , Node (TS.TSNode "55%" "55% Brightness" (brightness 0.55)) []
      , Node (TS.TSNode "60%" "60% Brightness" (brightness 0.60)) []
      , Node (TS.TSNode "65%" "65% Brightness" (brightness 0.65)) []
      , Node (TS.TSNode "70%" "70% Brightness" (brightness 0.70)) []
      , Node (TS.TSNode "80%" "80% Brightness" (brightness 0.80)) []
      , Node (TS.TSNode "90%" "90% Brightness" (brightness 0.90)) []
      ]
   , Node (TS.TSNode "+ Color Temperature" "Adjust color temperature" (return ())) 
      [ Node (TS.TSNode "30 K" "30k temperature" (colorTemperature 30000)) []
      , Node (TS.TSNode "35 K" "35k temperature" (colorTemperature 35000)) []
      , Node (TS.TSNode "40 K" "40k temperature" (colorTemperature 40000)) []
      , Node (TS.TSNode "45 K" "45k temperature" (colorTemperature 45000)) [] 
      , Node (TS.TSNode "50 K" "50k temperature" (colorTemperature 50000)) []
      , Node (TS.TSNode "55 K" "55k temperature" (colorTemperature 55000)) []
      , Node (TS.TSNode "60 K" "60k temperature" (colorTemperature 60000)) []
      , Node (TS.TSNode "65 K" "65k temperature" (colorTemperature 65000)) []
      ]
   , Node (TS.TSNode "Shutdown" "Poweroff" (spawn "poweroff")) []
   ]
toggleMuteCmd = "amixer -q sset Master toggle"
volume percentage = spawn ("amixer -M set Master " ++ show percentage ++ "%")
brightness percentage = spawn ("redshift -P -o -l 24:84 -b " ++ show percentage)
colorTemperature temperature =   spawn ("redshift -P -l 24:84 -O "++ show temperature) 

tsDefaultConfig = TS.TSConfig { TS.ts_hidechildren = True
                              , TS.ts_background   = 0xdd282c34
                              , TS.ts_font         = "xft:Sans-10"
                              , TS.ts_node         = (0xffd0d0d0, 0xff1c1f24)
                              , TS.ts_nodealt      = (0xffd0d0d0, 0xff282c34)
                              , TS.ts_highlight    = (0xffffffff, 0xff755999)
                              , TS.ts_extra        = 0xffd0d0d0
                              , TS.ts_node_width   = 200
                              , TS.ts_node_height  = 20
                              , TS.ts_originX      = 100
                              , TS.ts_originY      = 100
                              , TS.ts_indent       = 80
                              , TS.ts_navigate     = myTreeNavigation
                              }


myTreeNavigation = M.fromList
    [ ((0, xK_Escape), TS.cancel)
    , ((0, xK_Return), TS.select)
    , ((0, xK_space),  TS.select)
    , ((0, xK_Up),     TS.movePrev)
    , ((0, xK_Down),   TS.moveNext)
    , ((0, xK_Left),   TS.moveParent)
    , ((0, xK_Right),  TS.moveChild)
    , ((0, xK_k),      TS.movePrev)
    , ((0, xK_j),      TS.moveNext)
    , ((0, xK_h),      TS.moveParent)
    , ((0, xK_l),      TS.moveChild)
    , ((0, xK_o),      TS.moveHistBack)
    , ((0, xK_i),      TS.moveHistForward)
    , ((0, xK_s),      TS.moveTo ["Shutdown"])
    ]


myAdditionalKeysP = [
  ("M-<Return>", promote),
  -- Replace dmenu with rofi
  ("M-p", spawn "rofi -width 30 -show drun -theme ~/.config/rofi/themes/nord/nord.rasi"),
   -- Named Scratchpad bindings
  ("M-C-<Return>", namedScratchpadAction myScratchPads "dropdownTerminal"),
  -- Layout modifier bindings
  ("M-S-s", sendMessage MirrorShrink ),
  ("M-S-x", sendMessage MirrorExpand ),
  -- Actions bindings
  ("M-S-t", sinkAll ),
  -- Xmonad actions
  ("M-S-q", confirmPrompt myXPConfig "exit" (io exitSuccess)),
  ("M-q",   spawn "xmonad --recompile && xmonad --restart"),
  -- Window bindings
  ("M-a"   , windows copyToAll ),
  ("M-C-a" , killAllOtherCopies),
  ("M-S-a" , kill1),
  ("M1-<Tab>"   , cycleRecentWindows [xK_Alt_L] xK_Tab xK_Tab ),
  -- Workspace bindings
  ("M-<Left>", moveTo Prev NonEmptyWS),
  ("M-<Right>", moveTo Next NonEmptyWS),
  ("M-<Tab>"   , cycleRecentWS [xK_Super_L] xK_Tab xK_Tab ),
  -- Prompts keybindings
  ("M-S-p", shellPrompt myXPConfig),
  ("M-s a", promptSearch myXPConfig amazon),
  ("M-s d", promptSearch myXPConfig duckduckgo),
  ("M-s g", promptSearch myXPConfig google),
  ("M-s i", promptSearch myXPConfig images),
  ("M-s m", promptSearch myXPConfig maps),
  ("M-s w", promptSearch myXPConfig wikipedia),
  ("M-s y", promptSearch myXPConfig youtube),
  -- Utilities and extensions
  ("M-x t", treeselectAction tsDefaultConfig),
  ("M-<Backspace>", spawn "feh --bg-fill $(find ~/Wallpapers | shuf -n 1)"),
 -- Multimedia key bindings
 ("M-<Up>",     spawn ("amixer set Master 5%+")),
 ("M-<Down>",   spawn ("amixer set Master 5%-")),
 ("M-S-<Up>",   spawn ("amixer set Capture 10%+")),
 ("M-S-<Down>", spawn ("amixer set Capture 10%-"))
 ]

myAdditionalMouseBindings = [
  ((myModMask, button3), (\w -> focus w >> Flex.mouseResizeWindow w))
 ]

