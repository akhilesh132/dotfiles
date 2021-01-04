import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import XMonad.Layout.ResizableTile
import XMonad.Util.Run(spawnPipe, hPutStrLn)
import XMonad.Util.SpawnOnce
import XMonad.Util.EZConfig
import XMonad.Wallpaper
import XMonad.Util.NamedScratchpad
import XMonad.Actions.CopyWindow
import XMonad.Actions.WithAll
import qualified  XMonad.StackSet as W

import Data.Monoid

main = do
   xmproc <- spawnPipe myBar
   setRandomWallpaper [ "$HOME/Wallpapers" ]
   xmonad $docks desktopConfig
      { borderWidth =     myBorderWidth
      , layoutHook =      myLayoutHook
      , manageHook =      myManageHook
      , handleEventHook = myHandleEventHook 
      , logHook =         myLogHook xmproc 
      , terminal   =      myTerminal
      , modMask    =      myModMask 
      , workspaces =      myWorkspaces
      } `additionalKeysP` myKeys

--Bind Mod to the Windows Key
myModMask = mod4Mask

myWorkspaces = [ "1", "2", "3", "4", "5", "6", "7", "8", "9" ]
myBorderWidth = 1
myTerminal = "alacritty"
myBar = "xmobar"

myLayoutHook =  spacingRaw True (Border 0 5 0 5) True (Border 5 0 5 0) True $
                resizableTallLayout ||| fullLayout ||| bottomLayout ||| myTabbedLayout
                where
                  resizableTallLayout = smartBorders . avoidStruts $ myResizableTall 
                  bottomLayout = smartBorders . avoidStruts $ Mirror( myResizableTall )
                  fullLayout = noBorders Full
                  myTabbedLayout = smartBorders simpleTabbed
                  myResizableTall = ResizableTall 1 (3/100) (1/2) []

myLogHook h = dynamicLogWithPP  xmobarPP {
    ppOutput  =        hPutStrLn h ,
    ppTitle   =        xmobarColor "lightgreen" "" . shorten 50,
    ppCurrent =        xmobarColor "yellow"     "" . shorten 10,
    ppHidden  =        xmobarColor "grey"       "" . shorten 10,
    ppUrgent  =        xmobarColor "red"        "" . shorten 10
  }

scratchpads = [
    NS "dropDownTerminal" spawnDropDownTerm findDropDownTerm manageDropDownTerm 
 ]
   where
     spawnDropDownTerm = "guake" 
     findDropDownTerm =   className =? ".guake-wrapped"
     manageDropDownTerm = customFloating $ W.RationalRect x y w h
                      where
                        x = 0.00
                        y = 0.00
                        w = 1.00
                        h = 0.40

myManageHook = composeAll [
    manageDocks,
    isFullscreen --> doFullFloat,
    floatingWindowsHook,
    namedScratchpadManageHook scratchpads,
    manageHook defaultConfig
 ] 

floatingWindowsHook = composeAll [
    className =? "Gimp" --> doFloat,
    className =? ".guake-wrapped" --> (doRectFloat $ W.RationalRect 0.00 0.00 1.00 0.40 ),
    className =? "TeamViewer"     --> (doRectFloat $ W.RationalRect 0.60 0.05 0.39 0.35),
    className =? "mpv"            --> (doRectFloat $ W.RationalRect 0.80 0.80 0.20 0.20)
 ]

myHandleEventHook = fullscreenEventHook

myKeys = [
  ("M-<Backspace>", spawn "feh --bg-fill $(find ~/Wallpapers | shuf -n 1)"),

   -- Named Scratchpad bindings
  ("M-g", namedScratchpadAction scratchpads "dropDownTerminal"),

  -- Layout modifier bindings
  ("M-S-s", sendMessage MirrorShrink ),
  ("M-S-x", sendMessage MirrorExpand ),

  -- Actions bindings
  ("M-S-t", sinkAll ),

  -- Window bindings
  ("M-a", windows copyToAll ),
  ("M-C-a" , killAllOtherCopies),
  ("M-S-a" , kill1)
 ]
