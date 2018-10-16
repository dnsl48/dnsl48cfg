import XMonad
import XMonad.Actions.Plane
import XMonad.Layout.Spacing

import Control.Monad
import Data.Monoid

import Graphics.X11.ExtraTypes.XF86

import System.Directory
import System.Exit
import System.FilePath
import System.Random
import System.IO


import qualified XMonad.StackSet as W
import qualified Data.Map        as M


data Dnsl48Workspaces = Dnsl48Workspaces {
    wsLines :: Int,
    wsSpaces :: Int
}

wsList :: Dnsl48Workspaces -> [String]
wsList a = [show i | i <- [1 .. ((wsLines a) * (wsSpaces a))]]

dnsl48Workspaces = Dnsl48Workspaces {
  wsLines = 3,
  wsSpaces = 4
}


main = xmonad defaultConfig
  {
    modMask = mod4Mask,
    borderWidth = 2,
    workspaces = wsList dnsl48Workspaces,
    keys = myKeys,
    terminal = "kitty",
    focusFollowsMouse = False,
    startupHook = changeBg
  }


-- sudoers:
-- ALL ALL = (root) NOPASSWD: abspath~/dnsl48cfg/xmonad/brightness.sh
brightness_sh dir val = "sudo ~/dnsl48cfg/xmonad/brightness.sh " ++ device ++ " " ++ dir ++ " " ++ val
--  where device = "radeon_bl0"
    where device = "intel_backlight"


-- key bindings
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.union (M.fromList $

  -- launch a terminal
  [
    ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

  , ((modm .|. shiftMask, xK_l), spawn "xlock -mode maze")

  -- launch dmenu
  , ((modm, xK_p), spawn "gmrun")

  , ((modm, xK_b), changeBg)

  -- close focused window
  , ((modm .|. shiftMask, xK_c), kill)

  -- Rotate through the available layout algorithms
  , ((modm, xK_space ), sendMessage NextLayout)

  --  Reset the layouts on the current workspace to default
  , ((modm .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)

  -- Resize viewed windows to the correct size
  -- , ((modm, xK_n), refresh)

  -- Move focus to the next window
  , ((modm, xK_Tab), windows W.focusDown)

  -- Move focus to the next window
  , ((modm, xK_n), windows W.focusDown)

  -- Move focus to the previous window
  , ((modm, xK_t), windows W.focusUp)

  -- Move focus to the master window
  , ((modm, xK_m), windows W.focusMaster)

  -- Swap the focused window and the master window
  , ((modm, xK_Return), windows W.swapMaster)

  -- Swap the focused window with the next window
  , ((modm .|. shiftMask, xK_n), windows W.swapDown)

  -- Swap the focused window with the previous window
  , ((modm .|. shiftMask, xK_t), windows W.swapUp)

  -- Shrink the master area
  , ((modm, xK_h), sendMessage Shrink)

  -- Expand the master area
  , ((modm, xK_l), sendMessage Expand)

  -- Push window back into tiling
  -- , ((modm, xK_t), withFocused $ windows . W.sink)

  -- Increment the number of windows in the master area
  , ((modm, xK_comma), sendMessage (IncMasterN 1))

  -- Deincrement the number of windows in the master area
  , ((modm, xK_period), sendMessage (IncMasterN (-1)))

  -- Toggle the status bar gap
  -- Use this binding with avoidStruts from Hooks.ManageDocks.
  -- See also the statusBar function from Hooks.DynamicLog.
  --
  -- , ((modm, xK_b), sendMessage ToggleStruts)

  -- Quit xmonad
  , ((modm .|. shiftMask, xK_q), io (exitWith ExitSuccess))

  -- Restart xmonad
  , ((modm, xK_q), spawn "xmonad --recompile; xmonad --restart")

  , ((noModMask, xF86XK_AudioMute), spawn "pacmd dump|awk --non-decimal-data '$1~/set-sink-mute/{system (\"pacmd \"$1\" \"$2\" \"($3==\"yes\"?\"no\":\"yes\"))}'")

  , ((noModMask, xF86XK_AudioLowerVolume), spawn "pacmd dump|awk --non-decimal-data '$1~/set-sink-volume/{system (\"pacmd \"$1\" \"$2\" \"$3-1000)}'")

  , ((noModMask, xF86XK_AudioRaiseVolume), spawn "pacmd dump|awk --non-decimal-data '$1~/set-sink-volume/{system (\"pacmd \"$1\" \"$2\" \"$3+1000)}'")

  , ((noModMask, xF86XK_MonBrightnessUp), spawn $ brightness_sh "up" "5")

  , ((noModMask, xF86XK_MonBrightnessDown), spawn $ brightness_sh "down" "5")

  , ((noModMask, xF86XK_Display), spawn $ brightness_sh "toggle" "")

  , ((noModMask, xK_Print), spawn "imlib2_grab ~/screenshot.png")
  ]

  ++

  --
  -- mod-{semicolon,comma}, Switch to physical/Xinerama screens 1 or 2
  -- mod-shift-{semicolon,comma}, Move client to screen 1 or 2
  --
  [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
      | (key, sc) <- zip [xK_semicolon, xK_comma] [0..]
      , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

  ++

  -- dvorak for programmers keybinding
  [
    ((m .|. modm, k), windows $ f i)
  | (i, k) <- zip (XMonad.workspaces conf) [xK_ampersand, xK_bracketleft, xK_braceleft, xK_braceright]
  , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ]) $ planeKeys modm (Lines $ wsLines dnsl48Workspaces) Circular


changeBg = spawn "feh --bg-fill ~/bgimages/$(ls ~/bgimages/ | sort -R | tail -n 1)"
