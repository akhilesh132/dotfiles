import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Layout.Spacing
import XMonad.Util.Run(spawnPipe)
import XMonad.Wallpaper

main = do
   xmproc <- spawnPipe myBar
   setRandomWallpaper [ "$HOME/Wallpapers" ]
   xmonad $docks desktopConfig
      { borderWidth = myBorderWidth
      , layoutHook = myLayoutHook
      , manageHook = myManageHook
      , handleEventHook = myHandleEventHook 
      , terminal   = myTerminal
      , modMask    = myModMask 
      , workspaces = myWorkspaces
      }

myBar = "xmobar"

myBorderWidth = 1

myLayoutHook =  smartBorders . avoidStruts $
                spacingRaw True (Border 0 5 0 5) True (Border 5 0 5 0) True $
                Tall 1 (3/100) (1/2) ||| Full

myManageHook = composeAll [
    manageDocks,
    isFullscreen --> doFullFloat,
    floatingRule,
    manageHook defaultConfig
 ] 

floatingRule = composeAll [
    className =? "mpv" --> doFloat,
    className =? "guake" --> doFloat
 ]

myHandleEventHook = fullscreenEventHook

myTerminal = "alacritty"

--Bind Mod to the Windows Key
myModMask = mod4Mask

myWorkspaces = [ "1", "2", "3", "4", "5", "6", "7", "8", "9" ]

