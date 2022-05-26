import XMonad
import Data.List
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.Font
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ICCCMFocus
import XMonad.Actions.GridSelect
import XMonad.Layout.IM
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import qualified XMonad.StackSet as W
import XMonad.Hooks.EwmhDesktops
import System.IO

-- make sure to edit paths to xmobar and .xmobarrc to match your system.
-- If xmobar is in your $PATH, and its config is in ~/.xmobarrc you don't
-- need the xmobar path or config file, use: xmproc <- spawnPipe "xmobar"

-- definitions
myManageHook = composeAll
    [ (role     =? "Gimp" <||> role =? "gimp-image-window") --> (ask >>= doF . W.sink)
    , className =? "uxrvt"       --> doShift "1:dev"
    , className =? "chromium"    --> doShift "2:web"
    , className =? "firefox"     --> doShift "2:web"
    , className =? "Evince"      --> doShift "3:doc"
    , className =? "Gimp"        --> doShift "4:prog" 
    , className =? "qutebrowser" --> doShift "4:prog"
    , className =? "signal-desktop" --> doShift "4:prog"
    , className =? "Thunderbird" --> doShift "5:mail"
    , manageDocks
    ]
  where role = stringProperty "WM_WINDOW_ROLE"
myTerminal = "urxvt"
myHandleEventHook = handleEventHook defaultConfig <+> docksEventHook
myWorkSpaces = ["1:dev","2:web","3:doc","4:prog","5:mail","6:comp","7","8","9"]
myKeys = [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
         , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
         , ((0, xK_Print), spawn "scrot")
 	 , ((mod4Mask, xK_g), goToSelected defaultGSConfig)
	 , ((0, 0x1008ff59), spawn "xrandr --output DP1-1 --left-of eDP1 --mode 1920x1080 --output DP1-3 --mode 1680x1050 --left-of DP1-1")
	 , ((mod4Mask .|. shiftMask, xK_u), spawn "xrandr --output DP1-1 --off --output DP1-3 --off")
	 , ((mod4Mask, xK_b), sendMessage ToggleStruts)
	 , ((controlMask, xK_space), spawn "dmenu_run")
	 , ((0, 0x1008ff03), spawn "xbacklight -dec 10")
	 , ((0, 0x1008ff02), spawn "xbacklight -inc 10")
	 , ((0, 0x1008ff11), spawn "pacmd dump|awk --non-decimal-data '$1~/set-sink-volume/{system (\"pacmd \"$1\" \"$2\" \"$3-1000)}'")
	 , ((0, 0x1008ff12), spawn "pacmd dump|awk --non-decimal-data '$1~/set-sink-mute/{system (\"pacmd \"$1\" \"$2\" \"($3==\"yes\"?\"no\":\"yes\"))}'")
	 , ((0, 0x1008ff13), spawn "pacmd dump|awk --non-decimal-data '$1~/set-sink-volume/{system (\"pacmd \"$1\" \"$2\" \"$3+1000)}'")
         , ((0, 0x1008ff41), spawn "pactl set-source-mute 1 toggle")
         , ((0, 0x1008ff8f), spawn "cheese")
	 , ((0, 0x1008ff06), spawn "/usr/local/bin/kb-light.py -")
	 , ((0, 0x1008ff05), spawn "/usr/local/bin/kb-light.py +")
         ]

main = do
    xmproc <- spawnPipe "xmobar"
    xmonad$ defaultConfig
       {  workspaces = myWorkSpaces
        , terminal = "urxvt -e zsh"
	, borderWidth = 1
   	, normalBorderColor  = "black"
	, focusedBorderColor = "#1b6800"
	, manageHook = myManageHook <+> manageHook defaultConfig
        , layoutHook = avoidStruts  $ layoutHook defaultConfig
	, handleEventHook = handleEventHook defaultConfig <+> docksEventHook
        , logHook =  dynamicLogWithPP xmobarPP
                          { ppOutput = hPutStrLn xmproc
                          , ppTitle = xmobarColor "green" "" . shorten 50
                          } 
	, startupHook = setWMName "LG3D"
        , modMask = mod4Mask
        } `additionalKeys` myKeys
