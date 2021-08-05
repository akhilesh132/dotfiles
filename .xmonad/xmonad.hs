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
import XMonad.Prompt
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.ConfirmPrompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Window
import qualified  XMonad.StackSet as W
import XMonad.Wallpaper


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
              $ onWorkspace "5" floatingLayout
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

myAdditionalKeysP = [
  ("M-<Return>", promote),
  -- Replace dmenu with rofi
  ("M-p", spawn "rofi -width 30 -show drun -theme ~/.config/rofi/themes/nord/nord.rasi"),
  ("M-o", spawn "rofi -width 30 -show window -theme ~/.config/rofi/themes/nord/nord.rasi"),
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
  ("M-<Left>"    , moveTo Prev NonEmptyWS),
  ("M-<Right>"   , moveTo Next NonEmptyWS),
  ("M-<Tab>"     , cycleRecentWS [xK_Super_L] xK_Tab xK_Tab ),
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
  ("M-<Backspace>", spawn "sh $HOME/.repo/styli.sh/styli.sh -w 1920 -h 1080 -s nature"),
  ("M-<Escape>", spawn "i3lock-color -c 222222"),
 -- Multimedia key bindings
 ("M-<Up>",                     spawn ("amixer set Master 5%+")),
 ("M-<Down>",                   spawn ("amixer set Master 5%-")),
 ("M-S-<Up>",                   spawn ("amixer set Capture 10%+")),
 ("M-S-<Down>",                 spawn ("amixer set Capture 10%-")),
 ("M-C-<Up>",                   spawn ("xbrightness +1000 +1000 +1000")),
 ("M-C-<Down>",                 spawn ("xbrightness -1000 -1000 -1000")),
 ("M-M1-<Down>",                spawn ("xbrightness -500 -1500 -2000")),
 ("M-M1-<Up>",                  spawn ("xbrightness +500 +1500 +2000")),
 ("<XF86AudioLowerVolume>",     spawn ("amixer set Master 5%-")),
 ("<XF86AudioRaiseVolume>",     spawn ("amixer set Master 5%+")),
 ("<XF86AudioMute>",            spawn ("amixer -q sset Master toggle")),
 ("<XF86AudioPlay>",     spawn ("playerctl play-pause")),
 ("<XF86AudioStop>",     spawn ("playerctl stop")),
 ("<XF86AudioPrev>",     spawn ("playerctl previous")),
 ("<XF86AudioNext>",     spawn ("playerctl next"))
 ]

myAdditionalMouseBindings = [
  ((myModMask, button3), (\w -> focus w >> Flex.mouseResizeWindow w))
 ]

