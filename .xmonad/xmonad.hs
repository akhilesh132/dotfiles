import XMonad
import System.Exit
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
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
import XMonad.Wallpaper
import Data.Monoid
import Data.Tree
import qualified Data.Map as M


main = do
    spawn "${HOME}/.bin/picom --experimental-backends -b &"
    spawn "xbrightness 65535"
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
myWorkspaces = [ "1", "2", "3", "4", "5", "6", "7", "8", "9" ]
myBorderWidth = 1
myTerminal = "alacritty"
myFocusFollowsMouse = True
myClickJustFocuses = False

myLayoutHook =  onWorkspace "1"
                ( tallLayout
                  |||fullLayout 
                  ||| mirrorTallLayout 
                )
              $ onWorkspace "2" 
                ( fullLayout
                  ||| tallLayout 
                )
              $ onWorkspace "3" floatingLayout
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

myManageHook = composeOne [
    transience      --move transient windows to their parent
  , isDialog        -?> doCenterFloat
  , isFullscreen    -?> doFullFloat
  ] <+> composeAll [
    manageDocks,
    manageHook defaultConfig,
    floatingWindowsHook,
    fullscreenManageHook,
    namedScratchpadManageHook myScratchPads
 ] 

floatingWindowsHook = composeAll [
    className =? "Gimp"           --> doFloat,
    className =? "zoom"           --> doFloat,
    className =? "TeamViewer"     --> doFloat,
    className =? "mpv"            --> (doRectFloat $ W.RationalRect 0.80 0.80 0.20 0.20),
    className =? "dropdownTerminal" --> (doRectFloat $ W.RationalRect 0.25 0.25 0.25 0.25)
 ]

myHandleEventHook = fullscreenEventHook

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
   [ Node (TS.TSNode "+ Sound" "Adjust system audio" (return ())) 
      [ Node (TS.TSNode "Toggle Mute" "Toogle Mute" (spawn toggleSoundMuteCmd)) []
      , Node (TS.TSNode "90%" "90% Volume" (soundVolume 90)) []
      , Node (TS.TSNode "80%" "80% Volume" (soundVolume 80)) []
      , Node (TS.TSNode "70%" "70% Volume" (soundVolume 70)) []
      , Node (TS.TSNode "60%" "60% Volume" (soundVolume 60)) []
      , Node (TS.TSNode "50%" "50% Volume" (soundVolume 50)) []
      , Node (TS.TSNode "40%" "40% Volume" (soundVolume 40)) []
      , Node (TS.TSNode "30%" "30% Volume" (soundVolume 30)) []
      , Node (TS.TSNode "20%" "20% Volume" (soundVolume 20)) []
      ]
   , Node (TS.TSNode "+ Microphone" "Adjust system microphone" (return ())) 
      [ Node (TS.TSNode "Toggle Mute" "Toogle Mute" (spawn toggleMicrophoneMuteCmd)) []
      , Node (TS.TSNode "90%" "90% Volume" (microphoneVolume 90)) []
      , Node (TS.TSNode "80%" "80% Volume" (microphoneVolume 80)) []
      , Node (TS.TSNode "70%" "70% Volume" (microphoneVolume 70)) []
      , Node (TS.TSNode "60%" "60% Volume" (microphoneVolume 60)) []
      , Node (TS.TSNode "50%" "50% Volume" (microphoneVolume 50)) []
      , Node (TS.TSNode "40%" "40% Volume" (microphoneVolume 40)) []
      , Node (TS.TSNode "30%" "30% Volume" (microphoneVolume 30)) []
      , Node (TS.TSNode "20%" "20% Volume" (microphoneVolume 20)) []
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
      [ Node (TS.TSNode "3000 K" "3000k temperature" (colorTemperature 3000)) []
      , Node (TS.TSNode "3500 K" "3500k temperature" (colorTemperature 3500)) []
      , Node (TS.TSNode "4000 K" "4000k temperature" (colorTemperature 4000)) []
      , Node (TS.TSNode "4500 K" "4500k temperature" (colorTemperature 4500)) []
      , Node (TS.TSNode "5000 K" "5000k temperature" (colorTemperature 5000)) []
      , Node (TS.TSNode "6000K" "6000k temperature" (colorTemperature 6000)) [] 
      , Node (TS.TSNode "10000K" "10000k temperature" (colorTemperature 10000)) []
      , Node (TS.TSNode "15000K" "15000k temperature" (colorTemperature 15000)) []
      , Node (TS.TSNode "20000K" "20000k temperature" (colorTemperature 20000)) []
      , Node (TS.TSNode "25000K" "25000k temperature" (colorTemperature 25000)) []
      ]
   , Node (TS.TSNode "Shutdown" "Poweroff" (spawn "poweroff")) []
   , Node (TS.TSNode "Reboot"   "Reboot"   (spawn "reboot")) []
   ]

toggleSoundMuteCmd = "amixer -q sset Master toggle"
toggleMicrophoneMuteCmd = "amixer -q sset Capture toggle"

soundVolume percentage = spawn ("amixer -M set Master " ++ show percentage ++ "%")
microphoneVolume percentage = spawn ("amixer -M set Capture " ++ show percentage ++ "%")

brightness percentage = spawn ("redshift -P -o -l 24:84 -b " ++ show percentage)
colorTemperature temperature =   spawn ("redshift -P -l 24:84 -O "++ show temperature ++ "k") 

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
  ("M-<Backspace>", spawn "sh /home/akhilesh/.repo/styli.sh/styli.sh -w 1920 -h 1080"),
  ("M-<Escape>", spawn "i3lock-color -c 222222"),
 -- Multimedia key bindings
 ("M-<Up>",                     spawn ("amixer set Master 5%+")),
 ("<XF86AudioRaiseVolume>",     spawn ("amixer set Master 5%+")),
 ("<XF86AudioMute>",            spawn ("amixer -q sset Master toggle")),
 ("M-<Down>",                   spawn ("amixer set Master 5%-")),
 ("<XF86AudioLowerVolume>",     spawn ("amixer set Master 5%-")),
 ("M-S-<Up>",                   spawn ("amixer set Capture 10%+")),
 ("M-S-<Down>",                 spawn ("amixer set Capture 10%-")),
 ("M-C-<Up>",                   spawn ("xbrightness +1000")),
 ("M-C-<Down>",                 spawn ("xbrightness -1000")),
 ("<XF86AudioPlay>",     spawn ("playerctl play-pause")),
 ("<XF86AudioStop>",     spawn ("playerctl stop")),
 ("<XF86AudioPrev>",     spawn ("playerctl previous")),
 ("<XF86AudioNext>",     spawn ("playerctl next"))
 ]

myAdditionalMouseBindings = [
  ((myModMask, button3), (\w -> focus w >> Flex.mouseResizeWindow w))
 ]

