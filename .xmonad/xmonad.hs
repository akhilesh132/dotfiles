import XMonad
import System.Exit
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import XMonad.Layout.ResizableTile
import XMonad.Layout.Renamed
import XMonad.Util.Run(spawnPipe, hPutStrLn)
import XMonad.Util.SpawnOnce
import XMonad.Util.EZConfig
import XMonad.Wallpaper
import XMonad.Util.NamedScratchpad
import XMonad.Actions.CopyWindow
import XMonad.Actions.WithAll
import XMonad.Actions.Search
import XMonad.Prompt
import XMonad.Prompt.ConfirmPrompt
import XMonad.Prompt.Shell
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

myLayoutHook =  spacingRaw True (Border 0 5 0 5) True (Border 5 0 5 0) True 
                $   rightTileLayout
                ||| fullLayout
                ||| bottomLayout
                ||| tabbedLayout
                
rightTileLayout = renamed [Replace "Tall"]
       $ smartBorders
       $ avoidStruts
       $ ResizableTall 1 (3/100) (1/2) []
fullLayout = renamed [ Replace "Maximized"]
	   $ noBorders
	   $ Full
bottomLayout = renamed [Replace "Bottom"]
	   $ smartBorders
       $ avoidStruts 
       $ Mirror(ResizableTall 1 (3/100) (1/2) [])
tabbedLayout = renamed [Replace "Tabbed"]
	   $ smartBorders 
	   $ simpleTabbed
       
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

myManageHook = composeOne
  [
    transience      --move transient windows to their parent
  , isDialog        -?> doCenterFloat
  , isFullscreen    -?> doFullFloat
  ] <+> composeAll
  [
    manageDocks,
    floatingWindowsHook,
    namedScratchpadManageHook scratchpads,
    manageHook defaultConfig
 ] 

floatingWindowsHook = composeAll [
    className =? "Gimp"           --> doFloat,
    className =? "zoom"           --> doFloat,
    className =? "TeamViewer"     --> doFloat,
    className =? ".guake-wrapped" --> (doRectFloat $ W.RationalRect 0.00 0.00 1.00 0.40 ),
    className =? "mpv"            --> (doRectFloat $ W.RationalRect 0.80 0.80 0.20 0.20)
 ]

myHandleEventHook = fullscreenEventHook

--------------------------------------------------------------------------------
-- | Customize the way 'XMonad.Prompt' looks and behaves.  It's a
-- great replacement for dzen.
myXPConfig = def
  { position          = Top
  , alwaysHighlight   = True
  , promptBorderWidth = 0
  , font              = "xft:monospace:size=9"
  , fgColor           = "yellow"
  }

--------------------------------------------------------------------------------

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
  ("M-S-a" , kill1),

  -- Xmonad actions
  ("M-S-q", confirmPrompt myXPConfig "exit" (io exitSuccess)),
 
  -- Prompts keybindings
  ("M-S-p", shellPrompt myXPConfig),
  --Search engines
  ("M-x a", promptSearch myXPConfig amazon),
  ("M-x d", promptSearch myXPConfig duckduckgo),
  ("M-x g", promptSearch myXPConfig google),
  ("M-x i", promptSearch myXPConfig images),
  ("M-x m", promptSearch myXPConfig maps),
  ("M-x w", promptSearch myXPConfig wikipedia),
  ("M-x y", promptSearch myXPConfig youtube)
 ]
