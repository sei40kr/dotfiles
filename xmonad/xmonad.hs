import           System.IO
import           XMonad
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Layout.LayoutModifier
import           XMonad.Layout.Spacing
import qualified XMonad.StackSet              as W
import           XMonad.Util.EZConfig
import           XMonad.Util.Run
import           XMonad.Util.SpawnOnce


-- xmonad.hs --- XMonad configuration file

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

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

-- Use golden ratio for window resizing
-- cf https://wiki.haskell.org/Xmonad/Config_archive/Octoploid%27s_xmonad.hs
myLayoutHook ::
     ModifiedLayout AvoidStruts (Choose (ModifiedLayout SmartSpacing Tall) (Choose (Mirror (ModifiedLayout SmartSpacing Tall)) Full)) Window
myLayoutHook = avoidStruts $ tiled ||| Mirror tiled ||| Full
  where
    tiled = smartSpacing 10 $ Tall nmaster delta ratio
    nmaster = 1
    ratio = toRational (2 / (1 + sqrt 5) :: Double)
    delta = 0.03

myLogHook :: Handle -> X ()
myLogHook h =
  dynamicLogWithPP
    xmobarPP
    { ppCurrent = xmobarColor myColorWhite "" . myWorkspaceTransform
    , ppVisible = xmobarColor myColorGray "" . myWorkspaceTransform
    , ppHidden = xmobarColor myColorGray "" . myWorkspaceTransform
    , ppHiddenNoWindows = const ""
    , ppUrgent = xmobarColor myColorGray "" . myWorkspaceTransform
    , ppSep = xmobarColor myColorGray "" "   |   "
    , ppWsSep = "   "
    , ppTitle = xmobarColor myColorWhite ""
    , ppOrder = \(ws:_:t:_) -> [ws, t]
    , ppOutput = hPutStrLn h
    }

myManageHookShift :: ManageHook
myManageHookShift = composeAll []

myManageHookFloat :: ManageHook
myManageHookFloat =
  composeAll
    [ className =? "feh" --> doCenterFloat
    , className =? "Zeal" --> doRectFloat (W.RationalRect 0.1 0.1 0.8 0.8)
    , title =? "urxvtc-float" --> doCenterFloat
    ]

myManageHookIgnore :: ManageHook
myManageHookIgnore = composeAll []

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "compton --backend glx --vsync opengl"
  spawnOnce "dunst"

myTerminal :: String
myTerminal = "urxvtc"

myWorkspaces :: [String]
myWorkspaces = ["1", "2"]

myWorkspaceTransform :: String -> String
myWorkspaceTransform ws =
  case ws of
    "1" -> "1:  <fn=1>\xf489</fn>"
    "2" -> "2:  <fn=1>\xf484</fn>"
    _   -> ws

main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $
    ewmh $
    docks
      def
      { borderWidth = myBorderWidth
      , focusedBorderColor = myFocusedBorderColor
      , focusFollowsMouse = myFocusFollowsMouse
      , layoutHook = myLayoutHook
      , logHook = myLogHook xmproc
      , manageHook =
          myManageHookShift <+>
          myManageHookFloat <+>
          myManageHookIgnore <+> manageDocks <+> manageHook def
      , normalBorderColor = myNormalBorderColor
      , startupHook = myStartupHook
      , terminal = myTerminal
      , workspaces = myWorkspaces
      } `additionalKeys`
    [ ((mod1Mask, xK_Return), spawn "urxvtc")
    , ((mod1Mask .|. shiftMask, xK_Return), spawn "urxvtc-float")
    , ((mod1Mask, xK_p), spawn "~/.dmenurc")
    , ((mod1Mask, xK_Print), spawn "xmonad-screenshot")
    ]
