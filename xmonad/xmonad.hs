import XMonad
import XMonad.Actions.CycleWS
import XMonad.Layout.Spacing

import Control.Monad
import Data.Monoid

import System.Directory
import System.Exit
import System.FilePath
import System.Random
import System.IO


import qualified XMonad.StackSet as W
import qualified Data.Map        as M


main = xmonad defaultConfig
  {
    modMask = mod4Mask,
    borderWidth = 2,
--    layoutHook = myLayout,
    workspaces = fst myWorkspaces,
    keys = myKeys,
    terminal = "lilyterm",
    focusFollowsMouse = False,
    startupHook = changeBg
  }


myLayout = tiled ||| Mirror tiled ||| Full
  where
    tiled = spacing 5 $ Tall nmaster delta ratio
    nmaster = 1
    ratio = 2 / 3
    delta = 5 / 100


-- (list of workspaces, amount of workspaces for each line)
myWorkspaces = 
  ([show i :: String | i <- [1 .. lines * count]], count)
  where
    lines = 3
    count = 4


-- key bindings
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

  -- launch a terminal
  [
    ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

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

  -- Prev workspace
  , ((modm, xK_Left), prevWS)

  -- Next workspace
  , ((modm, xK_Right), nextWS)

  -- Shift to prev workspace
  , ((modm .|. shiftMask, xK_Left), shiftToPrev >> prevWS)

  -- Shift to next workspace
  , ((modm .|. shiftMask, xK_Right), shiftToNext >> nextWS)

  -- 
  , ((modm, xK_Down), do replicateM (snd myWorkspaces) nextWS; return ())

  , ((modm, xK_Up), do replicateM (snd myWorkspaces) prevWS; return ())

  , ((modm .|. shiftMask, xK_Down), do replicateM 4 (shiftToPrev >> nextWS); return ())

  , ((modm .|. shiftMask, xK_Up), do replicateM 4 (shiftToPrev >> prevWS); return ())
  ]

  ++

  -- dvorak for programmers keybinding
  [
    ((m .|. modm, k), windows $ f i)
  | (i, k) <- zip (XMonad.workspaces conf) [xK_ampersand, xK_bracketleft, xK_braceleft, xK_braceright]
  , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ]


changeBg = spawn "feh --bg-fill /home/dns/bgimages/$(ls /home/dns/bgimages/ | sort -R | tail -n 1)"