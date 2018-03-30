import           System.IO
import           XMonad
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Layout.LayoutModifier
import           XMonad.Layout.NoBorders
import           XMonad.Util.Run              (spawnPipe)

myBorderWidth :: Dimension
myBorderWidth = 2

myTerminal :: String
myTerminal = "urxvtc"

myLayoutHook :: ModifiedLayout
  AvoidStruts
  (ModifiedLayout SmartBorder (Choose Full (Mirror Tall)))
  Window
myLayoutHook = avoidStruts (smartBorders (Full ||| Mirror tiled))
  where
    tiled = Tall nmaster delta ratio
    nmaster = 1
    ratio = toRational (2/(1+sqrt 5)::Double)
    delta = 0.03

myLogHook :: Handle -> X ()
myLogHook xmproc = dynamicLogWithPP xmobarPP
      { ppOutput = hPutStrLn xmproc
      , ppTitle = xmobarColor "green" "" . shorten 50
      }

myManageHook :: ManageHook
myManageHook = manageDocks <+> manageHook def

main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ ewmh $ docks def {
    borderWidth = myBorderWidth
    , terminal = myTerminal
    , layoutHook = myLayoutHook
    , logHook = myLogHook xmproc
    , manageHook = myManageHook
  }
