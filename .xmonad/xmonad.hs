import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Layout.Spacing
import XMonad.Util.Run(spawnPipe, hPutStrLn)
import XMonad.Util.SpawnOnce
import XMonad.Util.EZConfig
import XMonad.Wallpaper
import XMonad.Util.NamedScratchpad

main = do
   xmproc <- spawnPipe myBar
   setRandomWallpaper [ "$HOME/Wallpapers" ]
   xmonad $docks desktopConfig
      { borderWidth = myBorderWidth
      , layoutHook = myLayoutHook
      , manageHook = myManageHook
      , handleEventHook = myHandleEventHook 
      , logHook = dynamicLogWithPP $ xmobarPP { ppOutput = hPutStrLn xmproc }
      , terminal   = myTerminal
      , modMask    = myModMask 
      , workspaces = myWorkspaces
      } `additionalKeysP` myKeys

--Bind Mod to the Windows Key
myModMask = mod4Mask

myWorkspaces = [ "1", "2", "3", "4", "5", "6", "7", "8", "9" ]
myBorderWidth = 1
myTerminal = "alacritty"
myBar = "xmobar -d"

myLayoutHook =  spacingRaw True (Border 0 5 0 5) True (Border 5 0 5 0) True $
                tallLayout ||| fullLayout ||| bottomLayout
                where
                  tallLayout = smartBorders . avoidStruts $ Tall 1 (3/100) (1/2)
                  bottomLayout = smartBorders . avoidStruts $ Mirror( Tall 1 (3/100) (1/2))
                  fullLayout = noBorders Full

scratchpads = [
    NS "guake" "guake" (className =? "guake") defaultFloating
 ]

myManageHook = composeAll [
    manageDocks,
    isFullscreen --> doFullFloat,
    floatingWindowsHook,
    namedScratchpadManageHook scratchpads,
    manageHook defaultConfig
 ] 

floatingWindowsHook = composeAll [
    className =? "mpv" --> doFloat,
    className =? "Guake" --> doFloat
 ]

myHandleEventHook = fullscreenEventHook

myKeys = [
  ("M-<Backspace>", spawn "feh --bg-scale $(find ~/Wallpapers | shuf -n 1)"),
  ("M-g", namedScratchpadAction scratchpads "guake")
 ]
