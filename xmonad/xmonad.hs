module Main (main) where

import           Control.Monad
import           System.IO
import           XMonad
import           XMonad.Actions.Navigation2D
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Layout.Gaps
import           XMonad.Layout.IndependentScreens
import           XMonad.Layout.Spacing
import           XMonad.Layout.ThreeColumns
import qualified XMonad.StackSet                  as W
import           XMonad.Util.EZConfig
import           XMonad.Util.Run
import           XMonad.Util.SpawnOnce


-- xmonad.hs
-- author: Seong Yong-ju <sei40kr@gmail.com>


--- Colors

myColorBlack :: String
myColorBlack = "#222222"
myColorBlue :: String
myColorBlue = "#0088cc"
myColorGray :: String
myColorGray = "#999999"
myColorWhite :: String
myColorWhite = "#ffffff"

myNormalBorderColor :: String
myNormalBorderColor = myColorGray
myFocusedBorderColor :: String
myFocusedBorderColor = myColorBlue


--- XMonad

myBorderWidth :: Dimension
myBorderWidth = 3

myLogHook :: [Handle] -> X ()
myLogHook hs =
  dynamicLogWithPP
    xmobarPP
    { ppCurrent = xmobarColor "#0088cc" "" . const "■"
    , ppVisible = xmobarColor myColorWhite "" . const "■"
    , ppHidden = xmobarColor myColorWhite "" . const "■"
    , ppHiddenNoWindows = xmobarColor myColorWhite "" . const "■"
    , ppUrgent = xmobarColor myColorWhite "" . const "■"
    , ppSep = xmobarColor myColorGray "" "   |   "
    , ppWsSep = " "
    , ppTitle = xmobarColor myColorWhite ""
    , ppOrder = \(ws:layout:t:_) -> [ws, layout, t]
    , ppOutput = forM_ hs . flip hPutStrLn
    }

myManageHookShift :: ManageHook
myManageHookShift = composeAll []

myManageHookFloat :: ManageHook
myManageHookFloat =
  composeAll
    [ className =? "feh" --> doCenterFloat
    , className =? "Zeal" --> doRectFloat (W.RationalRect 0.1 0.1 0.8 0.8)
    ]

myManageHookIgnore :: ManageHook
myManageHookIgnore = composeAll []

myWorkspaces :: [String]
myWorkspaces = ["1", "2"]

main :: IO ()
main = do
  nScreens <- countScreens :: IO Integer
  xmprocs <- mapM (spawnPipe . ("xmobar -x" ++) . show) [0 .. (nScreens - 1)]
  xmonad $
    docks $
    def
      { borderWidth = myBorderWidth
      , focusedBorderColor = myFocusedBorderColor
      , focusFollowsMouse = False
      , layoutHook =
          avoidStruts $
          smartSpacing 8 $
          gaps
            [(U, 8), (D, 8), (R, 12), (L, 12)]
            (ThreeCol 1 (3 / 100) (1 / 2) ||| ThreeColMid 1 (3 / 100) (1 / 2)) |||
          Full
      , logHook = myLogHook xmprocs
      , manageHook =
          myManageHookShift <+>
          myManageHookFloat <+>
          myManageHookIgnore <+> manageDocks <+> manageHook def
      , modMask = mod4Mask
      , normalBorderColor = myNormalBorderColor
      , terminal = "alacritty"
      , workspaces = myWorkspaces
      , startupHook =
          do spawnOnce "redshift"
             spawnOnce "fcitx-autostart"
      } `additionalKeys`
    [ ((mod4Mask, xK_j), windows W.focusDown)
    , ((mod4Mask, xK_k), windows W.focusUp)
    , ((mod4Mask, xK_Return), spawn "alacritty")
    -- Screen capturing
    , ((0, xK_Print), spawn "scrot '%Y-%m-%d-%T-screenshot.png'")
    , ((shiftMask, xK_Print), spawn "scrot -s '%Y-%m-%d-%T-screenshot.png'")
    ] `additionalKeysP`
    -- Remote Control
    [ ("<XF86AudioPlay>", spawn "cplay")
    , ("<XF86AudioNext>", spawn "cmus-remote -n")
    , ("<XF86AudioPrev>", spawn "cmus-remote -r")
    -- Change volume or toggle mute
    , ("<XF86AudioRaiseVolume>", spawn "amixer -q -D pulse sset Master 5%+")
    , ("<XF86AudioLowerVolume>", spawn "amixer -q -D pulse sset Master 5%-")
    , ("<XF86AudioMute>", spawn "amixer -q -D pulse sset Master toggle")
    ]
