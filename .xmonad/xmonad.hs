import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Wallpaper

main = do
   xmproc <- spawnPipe "xmobar"
   setRandomWallpaper [ "$HOME/Wallpapers" ]
   xmonad $docks desktopConfig
      { layoutHook = avoidStruts $ layoutHook defaultConfig
      , manageHook = manageHook def <+> manageDocks
      , terminal = "alacritty"
      , modMask = mod4Mask --Bind Mod to the Windows Key
      , workspaces = myWorkspaces
      }
     
myWorkspaces = [ "1", "2", "3", "4", "5" ]
