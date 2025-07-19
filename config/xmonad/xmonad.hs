{-# OPTIONS_GHC -Wall -Werror -Wno-missing-signatures -Wno-name-shadowing #-}

import Data.Map qualified as M
import Data.Monoid (All)
import System.Posix.User (getEffectiveUserName)
import System.Process (callProcess)
import XMonad
import XMonad.Actions.CopyWindow (copy)
import XMonad.Actions.CycleWS (nextWS, prevWS, shiftToNext, shiftToPrev)
import XMonad.Actions.Submap (submap)
import XMonad.Actions.ToggleFullFloat (toggleFullFloat, toggleFullFloatEwmhFullscreen)
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.FadeInactive (fadeInactiveLogHook)
import XMonad.Hooks.ManageDocks (avoidStruts, docks)
import XMonad.Hooks.ManageHelpers (doCenterFloat)
import XMonad.Layout.Groups (group)
import XMonad.Layout.Groups.Examples (zoomColumnIn, zoomColumnOut)
import XMonad.Layout.Groups.Helpers (focusDown, focusGroupDown, focusGroupUp, focusUp, swapDown, swapUp)
import XMonad.Layout.Groups.Wmii (moveToGroupDown, moveToGroupUp)
import XMonad.Layout.NoBorders (Ambiguity (OnlyScreenFloat), lessBorders)
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed (simpleTabbedAlways)
import XMonad.Layout.ZoomRow (zoomRow)
import XMonad.Prelude (find)
import XMonad.StackSet qualified as W
import XMonad.Util.Hacks (fixSteamFlicker)

myBorderWidth :: Dimension
myBorderWidth = 2

myWorkspaces :: [String]
myWorkspaces = ["1", "2", "3", "4"]

myLayout = avoidStruts $ lessBorders OnlyScreenFloat $ group simpleTabbedAlways $ mySpacing 32 16 zoomRow
 where
  mySpacing i j = spacingRaw False (Border i i i i) True (Border j j j j) True

myTerminal :: String
myTerminal = "sensible-terminal"

myModMask :: KeyMask
myModMask = mod4Mask

myExit :: X ()
myExit = do
  username <- io getEffectiveUserName
  io $ callProcess "loginctl" ["terminate-user", username]

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys (XConfig{XMonad.modMask = myModMask}) =
  M.fromList $
    [ -- Launch - Application
      ((myModMask, xK_space), spawn "rofi -modi drun -show drun")
    , -- Launch - Browser
      ((myModMask .|. shiftMask, xK_Return), spawn "sensible-browser")
    , -- Launch - Terminal
      ((myModMask, xK_Return), spawn myTerminal)
    , -- Modify - Window Position
      ((myModMask .|. shiftMask, xK_h), moveToGroupUp False)
    , ((myModMask .|. shiftMask, xK_j), swapUp)
    , ((myModMask .|. shiftMask, xK_k), swapDown)
    , ((myModMask .|. shiftMask, xK_l), moveToGroupDown False)
    , ((myModMask .|. shiftMask, xK_Left), moveToGroupUp False)
    , ((myModMask .|. shiftMask, xK_Down), swapDown)
    , ((myModMask .|. shiftMask, xK_Up), swapUp)
    , ((myModMask .|. shiftMask, xK_Right), moveToGroupDown False)
    , -- Modify - Containing Workspace
      ((myModMask .|. controlMask .|. shiftMask, xK_Left), shiftToPrev)
    , ((myModMask .|. controlMask .|. shiftMask, xK_Right), shiftToNext)
    , -- Modify - Tile/Float Focus Toggle
      ((myModMask .|. shiftMask, xK_t), toggleFocusFloating)
    , -- Modify - Window Floating Toggle
      ((myModMask .|. shiftMask, xK_f), toggleFloat)
    , -- Modify - Window Fullscreen Toggle
      ((myModMask, xK_f), withFocused toggleFullFloat)
    , -- Navigate - Next Workspace
      ((myModMask .|. mod1Mask, xK_Right), nextWS)
    , ((myModMask, xK_Tab), nextWS)
    , -- Navigate - Previous Workspace
      ((myModMask .|. mod1Mask, xK_Left), prevWS)
    , ((myModMask .|. shiftMask, xK_Tab), prevWS)
    , -- Navigate - Relative Window
      ((myModMask, xK_h), focusGroupUp)
    , ((myModMask, xK_j), focusDown)
    , ((myModMask, xK_k), focusUp)
    , ((myModMask, xK_l), focusGroupDown)
    , ((myModMask, xK_Left), focusGroupUp)
    , ((myModMask, xK_Down), focusDown)
    , ((myModMask, xK_Up), focusUp)
    , ((myModMask, xK_Right), focusGroupDown)
    , -- Resize - Enter Resize Mode
      ((myModMask, xK_r), resizeMode)
    , -- Session - Exit App
      ((myModMask .|. shiftMask, xK_q), kill)
    , -- Session - Logout
      ((myModMask .|. shiftMask, xK_e), myExit)
    , -- Session - Refresh Session
      ((myModMask .|. shiftMask, xK_r), rescreen)
    , -- Session - Restart XMonad
      ((myModMask .|. controlMask, xK_r), restart "xmonad" True)
    ]
      ++ [ ((m .|. myModMask, k), windows $ f i)
         | (i, k) <- zip myWorkspaces [xK_1 ..]
         , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask), (copy, controlMask)]
         ]
 where
  toggleFloat = withFocused $ \w -> windows $ \s ->
    if M.member w (W.floating s)
      then W.sink w s
      else W.float w (W.RationalRect (1 / 4) (1 / 4) (1 / 2) (1 / 2)) s
  toggleFocusFloating = windows $ \s ->
    case W.peek s of
      Nothing -> s
      Just w ->
        let currentWindows = maybe [] W.integrate . W.stack . W.workspace . W.current $ s
         in if M.member w (W.floating s)
              then fromMaybe s $ do
                t <- find (\w' -> not $ M.member w' (W.floating s)) currentWindows
                return $ W.focusWindow t s
              else fromMaybe s $ do
                t <- find (\w' -> M.member w' (W.floating s)) currentWindows
                return $ W.focusWindow t s
   where
    fromMaybe x Nothing = x
    fromMaybe _ (Just x) = x
  resizeMode =
    submap $
      M.fromList
        [ ((0, xK_h), zoomColumnOut >> resizeMode)
        , ((0, xK_l), zoomColumnIn >> resizeMode)
        , ((0, xK_Left), zoomColumnOut >> resizeMode)
        , ((0, xK_Right), zoomColumnIn >> resizeMode)
        , ((0, xK_Return), return ())
        , ((0, xK_Escape), return ())
        , ((myModMask, xK_r), return ())
        ]

myLogHook :: X ()
myLogHook = fadeInactiveLogHook 0.9

myManageHook :: ManageHook
myManageHook =
  composeAll
    [ className =? "firefox" --> doShift wsWeb
    , className =? "Google-chrome" --> doShift wsWeb
    , -- Bitwarden browser extension
      appName =? "crx_nngceckbapebfimnlniiiahkandclblb" --> doCenterFloat
    , className =? "Vivaldi-stable" --> doShift wsWeb
    , className =? "Vivaldi-stable" <&&> title =? "Bitwarden - Vivaldi" --> doCenterFloat
    , className =? "jetbrains-idea" --> doShift wsDev
    , className =? "jetbrains-datagrip" --> doShift wsDev
    , className =? "jetbrains-dataspell" --> doShift wsDev
    , className =? "kitty" --> doShift wsDev
    , className =? "Matplotlib" --> doShift wsDev <+> doCenterFloat
    , className =? "Zeal" --> doShift wsDev <+> doFloat
    , className =? "org.wezfurlong.wezterm" --> doShift wsDev
    , className =? "file-roller" --> doShift wsFiles
    , className =? "Ristretto" --> doShift wsFiles
    , className =? "Thunar" --> doShift wsFiles
    , className =? "vlc" --> doShift wsFiles
    , className =? "Zathura" --> doShift wsFiles
    , className =? "discord" --> doShift wsIM
    , className =? "Slack" --> doShift wsIM
    , className =? "zoom" --> doShift wsIM
    , className =? "Bitwarden" --> doFloat
    , className =? "fcitx5-config-qt" --> doFloat
    , className =? "Gcr-prompter" --> doCenterFloat
    , className =? "Gnome-pomodoro" --> doCenterFloat
    ]
 where
  wsWeb = "1"
  wsDev = "2"
  wsFiles = "3"
  wsIM = "4"

myHandleEventHook :: Event -> X All
myHandleEventHook = fixSteamFlicker

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
        , manageHook = myManageHook
        , handleEventHook = myHandleEventHook
        , focusFollowsMouse = False
        , keys = myKeys
        }
