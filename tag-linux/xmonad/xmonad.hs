import XMonad
import XMonad.Hooks.EwmhDesktops

myBorderWidth :: Dimension
myBorderWidth = 2

myTerminal :: String
myTerminal = "urxvt"

myLayoutHook :: Choose Tall (Choose (Mirror Tall) Full) a
myLayoutHook = tiled ||| Mirror tiled ||| Full
  where
    tiled = Tall nmaster delta ratio
    nmaster = 1
    ratio = toRational (2/(1+sqrt(5)::Double))
    delta = 3 / 100

main :: IO ()
main = do
  xmonad $ ewmh def {
    borderWidth = myBorderWidth
    , terminal = myTerminal
    , layoutHook = myLayoutHook
  }
