import XMonad
import System.Exit
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
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
import qualified XMonad.Actions.TreeSelect as TS
import XMonad.Prompt
import XMonad.Prompt.ConfirmPrompt
import XMonad.Prompt.Shell
import qualified  XMonad.StackSet as W

import Data.Monoid
import Data.Tree
import qualified Data.Map as M


main = do
   xmproc <- spawnPipe "xmobar ~/.xmobarrc"
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

myWorkspaces = [ "main", "web", "files", "dev", "collab", "6", "7", "8", "9" ]
myBorderWidth = 1
myTerminal = "alacritty"
myBar = "xmobar"

myLayoutHook =  onWorkspace "main" ( tallLayout ||| fullLayout ||| mirrorTallLayout )
              $ onWorkspace "web" ( fullLayout ||| tallLayout )
              $ onWorkspace "collab" floatingLayout
              $ tallLayout ||| fullLayout
 
tallLayout = renamed [Replace "Tall"]
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

--  Customize the way 'XMonad.Prompt' looks and behaves.
--  It's a great replacement for dzen.
myXPConfig = def
  { position          = Top
  , alwaysHighlight   = True
  , promptBorderWidth = 0
  , font              = "xft:monospace:size=9"
  , fgColor           = "yellow"
  }

treeselectAction a = TS.treeselectAction a
   [ Node (TS.TSNode "Shutdown" "Poweroff the system" (spawn "poweroff")) []
   , Node (TS.TSNode "Brightness" "Adjust system brightness" (return ())) 
      [ Node (TS.TSNode "Full"    "Full Brightness" (spawn (brightness "full")))    []
      , Node (TS.TSNode "Mid"     "Mid Brightness"  (spawn (brightness "mid")))     []
      , Node (TS.TSNode "Low"     "Low Brightness"  (spawn (brightness "low")))     []
      , Node (TS.TSNode "Reading" "Reading mode"    (spawn (brightness "reading"))) []
      , Node (TS.TSNode "Dark"    "Dark room"       (spawn (brightness "dark")))    []
        
      ]
   
   ]

blueLightFilter :: [Char]
blueLightFilter = "redshift -l 24:84 -b 1:1 -o"

brightness:: String -> String
brightness "full" = "xbrightness 65535"
brightness "mid" = "xbrightness 45000"
brightness "low" = "xbrightness 30000"
brightness "reading" = "xbrightness 32000; " ++ blueLightFilter
brightness "dark" = "xbrightness 30000; " ++ blueLightFilter

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
    ]

myKeys = [
  ("M-<Backspace>", spawn "feh --bg-fill $(find ~/Wallpapers | shuf -n 1)"),

  -- Replace dmenu with rofi
  ("M-p", spawn "rofi -width 30 -show drun -theme ~/.config/rofi/themes/nord/nord.rasi"),

   -- Named Scratchpad bindings
  ("M-g", namedScratchpadAction scratchpads "dropDownTerminal"),

  -- Layout modifier bindings
  ("M-S-s", sendMessage MirrorShrink ),
  ("M-S-x", sendMessage MirrorExpand ),

  -- Actions bindings
  ("M-S-t", sinkAll ),

  -- Window bindings
  ("M-a"   , windows copyToAll ),
  ("M-C-a" , killAllOtherCopies),
  ("M-S-a" , kill1),

  -- Xmonad actions
  ("M-S-q", confirmPrompt myXPConfig "exit" (io exitSuccess)),
 
  -- Prompts keybindings
  ("M-S-p", shellPrompt myXPConfig),
  --Search engines
  ("M-s a", promptSearch myXPConfig amazon),
  ("M-s d", promptSearch myXPConfig duckduckgo),
  ("M-s g", promptSearch myXPConfig google),
  ("M-s i", promptSearch myXPConfig images),
  ("M-s m", promptSearch myXPConfig maps),
  ("M-s w", promptSearch myXPConfig wikipedia),
  ("M-s y", promptSearch myXPConfig youtube),
  -- utilities and extensions
  ("M-x t", treeselectAction tsDefaultConfig)
 ]
