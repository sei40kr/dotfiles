import Data.Map qualified as M
import XMonad
import XMonad.Actions.CopyWindow (copy)
import XMonad.Actions.CycleWS (nextWS, prevWS)
import XMonad.Actions.Submap (submap)
import XMonad.Actions.ToggleFullFloat (toggleFullFloat, toggleFullFloatEwmhFullscreen)
import XMonad.Config.Desktop
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.FadeInactive (fadeInactiveLogHook)
import XMonad.Hooks.ManageDocks (avoidStruts, docks)
import XMonad.Layout.Column (Column (Column))
import XMonad.Layout.Gaps
import XMonad.Layout.Groups (GroupsMessage (ModifyX), group)
import XMonad.Layout.Groups.Helpers (focusDown, focusGroupDown, focusGroupUp, focusUp, swapDown, swapUp)
import XMonad.Layout.Groups.Wmii (moveToGroupDown, moveToGroupUp, shrinkText, wmii)
import XMonad.Layout.MultiColumns (multiCol)
import XMonad.Layout.MultiToggle (EOT (EOT), Toggle (Toggle), mkToggle, mkToggle1, single)
import XMonad.Layout.MultiToggle.Instances (StdTransformers (MIRROR, NBFULL))
import XMonad.Layout.Renamed (named)
import XMonad.Layout.ResizableThreeColumns (MirrorResize (MirrorExpand, MirrorShrink), ResizableThreeCol (ResizableThreeCol))
import XMonad.Layout.Simplest (Simplest (Simplest))
import XMonad.Layout.Spacing
import XMonad.Layout.TabBarDecoration (simpleTabBar)
import XMonad.Layout.Tabbed (simpleTabbedAlways)
import XMonad.Layout.ToggleLayouts (toggleLayouts)
import XMonad.StackSet qualified as W
import XMonad.Util.EZConfig (additionalKeys, additionalKeysP)
import XMonad.Util.SpawnOnce

myBorderWidth :: Dimension
myBorderWidth = 2

myWorkspaces :: [String]
myWorkspaces = ["1", "2", "3", "4", "5"]

myLayout = avoidStruts $ group simpleTabbedAlways $ mySpacing 32 16 $ Mirror $ Column 1.0
 where
  mySpacing i j = spacingRaw False (Border i i i i) True (Border j j j j) True

myTerminal :: String
myTerminal = "sensible-terminal"

myModMask :: KeyMask
myModMask = mod4Mask

myLogHook = fadeInactiveLogHook 0.9

main :: IO ()
main =
  xmonad $
    ewmh . docks . toggleFullFloatEwmhFullscreen . ewmhFullscreen $
      def
        { borderWidth = myBorderWidth
        , workspaces = myWorkspaces
        , layoutHook = myLayout
        , terminal = myTerminal
        , modMask = myModMask
        , logHook = myLogHook
        , focusFollowsMouse = False
        }
        `additionalKeys` [
                           -- Exit app
                           ((myModMask .|. shiftMask, xK_q), kill)
                         , -- Restart XMonad
                           ((myModMask .|. controlMask, xK_r), restart "xmonad" True)
                         , -- Window Fullscreen Toggle
                           ((myModMask, xK_f), withFocused toggleFullFloat)
                         , -- Window Position
                           ((myModMask, xK_h), focusGroupUp)
                         , ((myModMask, xK_j), focusDown)
                         , ((myModMask, xK_k), focusUp)
                         , ((myModMask, xK_l), focusGroupDown)
                         , ((myModMask, xK_Left), focusGroupUp)
                         , ((myModMask, xK_Down), focusDown)
                         , ((myModMask, xK_Up), focusUp)
                         , ((myModMask, xK_Right), focusGroupDown)
                         , ((myModMask, xK_Tab), nextWS)
                         , ((myModMask .|. shiftMask, xK_Tab), prevWS)
                         , ((myModMask .|. shiftMask, xK_h), moveToGroupUp False)
                         , ((myModMask .|. shiftMask, xK_j), swapUp)
                         , ((myModMask .|. shiftMask, xK_k), swapDown)
                         , ((myModMask .|. shiftMask, xK_l), moveToGroupDown False)
                         , ((myModMask .|. shiftMask, xK_Left), moveToGroupUp False)
                         , ((myModMask .|. shiftMask, xK_Down), swapDown)
                         , ((myModMask .|. shiftMask, xK_Up), swapUp)
                         , ((myModMask .|. shiftMask, xK_Right), moveToGroupDown False)
                         , ((myModMask, xK_space), spawn "rofi -modi drun -show drun")
                         , ((myModMask .|. shiftMask, xK_Return), spawn myTerminal)
                         , ((myModMask, xK_Return), spawn "sensible-terminal")
                         ]
        `additionalKeys` [ ((m .|. myModMask, k), windows $ f i)
                         | (i, k) <- zip myWorkspaces [xK_1 ..]
                         , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask), (copy, controlMask)]
                         ]
