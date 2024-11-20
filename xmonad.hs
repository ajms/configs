import XMonad
import XMonad.Actions.GridSelect
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Layout.Magnifier
import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.SpawnOnce
import XMonad.Util.Run (spawnPipe)
import qualified XMonad.StackSet as W

main :: IO ()
main = do
  xmonad
    . ewmhFullscreen
    . ewmh
    . withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) defToggleStrutsKey
    $ myConfig

myConfig =
  def
    { modMask = mod4Mask, -- rebind mod to the super key
      workspaces = myWorkSpaces,
      terminal = myTerminal,
      borderWidth = 1,
      normalBorderColor = "grey",
      focusedBorderColor = "black",
      manageHook = myManageHook,
      startupHook = myStartupHook
    }
    `additionalKeys` myKeys

myTerminal = "alacritty"

myColorizer :: Window -> Bool -> X (String, String)
myColorizer w isFg = do
    className <- runQuery className w
    return $ case className of
      "firefox" -> if isFg then ("#000080", "#ffffff") else ("#8080ff", "#000000")  -- Light blue text on dark blue background when selected
      "chromium" -> if isFg then ("#000080", "#ffffff") else ("#8080ff", "#000000")  -- Light blue text on dark blue background when selected
      "thunderbird" -> if isFg then ("#1B7973", "#ffffff") else ("#85E0DC", "#000000")  -- Light turquoise text on dark turquoise background when selected
      "Alacritty" -> if isFg then ("#4C4C4C", "#ffffff") else ("#B2B2B2", "#000000")  -- Light grey text on dark grey background for terminal when selected
      "Code" -> if isFg then ("#800000", "#ffffff") else ("#ff8080", "#000000")  -- Light red text on dark red background when selected
      "Mattermost" -> if isFg then ("#800080", "#ffffff") else ("#cbaacb", "#000000")  -- Light grey text on dark grey background when selected
      "KeePassXC" -> if isFg then ("#006400", "#ffffff") else ("#80C080", "#000000")  -- Light green text on dark green background when selected
      "Postman" -> if isFg then ("#ffd700", "#ffffff") else ("#ffffe0", "#000000")  -- Light green text on dark green background when selected
      _ -> if isFg then ("#004000", "#ffffff") else ("#80FF80", "#000000")  -- Default: Light green text on dark green background when selected

myGridConfig :: GSConfig Window
myGridConfig = (buildDefaultGSConfig myColorizer)
    { gs_cellheight = 100
    , gs_cellwidth = 200
    , gs_cellpadding = 10
    , gs_font = "xft:Sans-15"
    }
        
myManageHook :: ManageHook
myManageHook =
  composeAll
    [ className =? "Gimp" --> doFloat,
      isDialog --> doFloat,
      className =? "Mattermost" --> doShift "pwd",
      className =? "Alacritty" --> doShift "term",
      className =? "Code" --> doShift "dev",
      className =? "chromium" --> doShift "web",
      className =? "firefox" --> doShift "web",
      className =? "Evince" --> doShift "doc",
      className =? "Gimp" --> doShift "9",
      className =? "thunderbird" --> doShift "mail",
      className =? "KeePassXC" --> doShift "pwd",
      className =? "obsidian" --> doShift "obs",
      className =? "Postman" --> doShift "post",
      manageDocks
    ]

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "alacritty -e nvtop"

myWorkSpaces = ["term", "web", "dev", "doc", "mail", "pwd", "post", "obs", "9"]

myKeys =
  [ ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s"),
    ((0, xK_Print), unGrab *> spawn "scrot -s"),
    ((mod4Mask, xK_g), goToSelected myGridConfig),
    ((0, 0x1008ff59), spawn "xrandr --output DP1-1 --left-of eDP1 --mode 1920x1080 --output DP1-3 --mode 1680x1050 --left-of DP1-1"),
    ((mod4Mask .|. shiftMask, xK_u), spawn "xrandr --output DP1-1 --off --output DP1-3 --off"),
    ((mod4Mask, xK_b), sendMessage ToggleStruts),
    ((controlMask .|. mod1Mask, xK_space), spawn "LIBGL_ALWAYS_SOFTWARE=1 dmenu_run"),
    ((mod4Mask, xK_l), spawn "xsecurelock"),
    ((0, 0x1008ff02), spawn "xbacklight -inc 10"),
    ((0, 0x1008ff03), spawn "xbacklight -dec 10"),
    ((0, 0x1008ff11), spawn "pacmd dump|awk --non-decimal-data '$1~/set-sink-volume/{system (\"pacmd \"$1\" \"$2\" \"$3-1000)}'"),
    ((0, 0x1008ff12), spawn "pacmd dump|awk --non-decimal-data '$1~/set-sink-mute/{system (\"pacmd \"$1\" \"$2\" \"($3==\"yes\"?\"no\":\"yes\"))}'"),
    ((0, 0x1008ff13), spawn "pacmd dump|awk --non-decimal-data '$1~/set-sink-volume/{system (\"pacmd \"$1\" \"$2\" \"$3+1000)}'"),
    ((0, 0x1008ff41), spawn "pactl set-source-mute 1 toggle"),
    ((0, 0x1008ff06), spawn "/usr/local/bin/kb-light.py -"),
    ((0, 0x1008ff05), spawn "/usr/local/bin/kb-light.py +")
  ]

myXmobarPP :: PP
myXmobarPP =
  def
    { ppSep = magenta " • ",
      ppTitleSanitize = xmobarStrip,
      ppCurrent = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2,
      ppHidden = white . wrap " " "",
      ppHiddenNoWindows = lowWhite . wrap " " "",
      ppUrgent = red . wrap (yellow "!") (yellow "!"),
      ppOrder = \[ws, l, _, wins] -> [ws, l, wins],
      ppExtras = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused = wrap (white "[") (white "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue . ppWindow

    -- Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta = xmobarColor "#ff79c6" ""
    blue = xmobarColor "#bd93f9" ""
    white = xmobarColor "#f8f8f2" ""
    yellow = xmobarColor "#f1fa8c" ""
    red = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""
