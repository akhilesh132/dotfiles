import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Util.Run(spawnPipe)
import XMonad.Wallpaper

main = do
   xmproc <- spawnPipe "xmobar"
   setRandomWallpaper [ "$HOME/Wallpapers" ]
   xmonad $docks desktopConfig
      { layoutHook = myLayoutHook
      , manageHook = manageHook def <+> manageDocks
      , terminal = "alacritty"
      , modMask = mod4Mask --Bind Mod to the Windows Key
      , workspaces = myWorkspaces
      }
myLayoutHook = smartBorders $ avoidStruts $ layoutHook defaultConfig
myWorkspaces = [ "1", "2", "3", "4", "5", "6", "7", "8", "9" ]
