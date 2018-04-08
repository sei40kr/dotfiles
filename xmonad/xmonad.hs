import           System.IO
import           XMonad
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Layout.LayoutModifier
import           XMonad.Layout.Spacing
import           XMonad.Util.EZConfig
import           XMonad.Util.Run


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
    { ppOrder = \(ws:_:t:_) -> [ws, t]
    , ppCurrent = xmobarColor myColorWhite myColorBlue . wrap "  " "  "
    , ppUrgent = xmobarColor myColorWhite "" . wrap "  " "  "
    , ppVisible = xmobarColor myColorBlue "" . wrap "  " "  "
    , ppHidden = xmobarColor myColorGray "" . wrap "  " "  "
    , ppHiddenNoWindows = const ""
    , ppTitle = xmobarColor myColorWhite "" . wrap "  " "  "
    , ppOutput = hPutStrLn h
    , ppWsSep = ""
    , ppSep = xmobarColor myColorGray "" " |"
    }

myManageHookShift :: ManageHook
myManageHookShift = composeAll []

myManageHookFloat :: ManageHook
myManageHookFloat =
  composeAll
    [ className =? "feh" --> doCenterFloat
    , title =? "urxvtc-float" --> doCenterFloat
    ]

myManageHookIgnore :: ManageHook
myManageHookIgnore = composeAll []

myTerminal :: String
myTerminal = "urxvtc"

myWorkspaces :: [String]
myWorkspaces = ["1", "2"]


main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $
    ewmh $
    docks
      def
      { borderWidth = myBorderWidth
      , focusedBorderColor = myFocusedBorderColor
      , layoutHook = myLayoutHook
      , logHook = myLogHook xmproc
      , manageHook =
          myManageHookShift <+>
          myManageHookFloat <+>
          myManageHookIgnore <+> manageDocks <+> manageHook def
      , normalBorderColor = myNormalBorderColor
      , terminal = myTerminal
      , workspaces = myWorkspaces
      } `additionalKeysP`
    [ ("M-<Return>", spawn "urxvtc")
    , ("M-S-<Return>", spawn "urxvtc-float")
    , ("M-p", spawn "~/.dmenurc")
    ]
