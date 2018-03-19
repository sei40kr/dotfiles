import XMonad

myBorderWidth :: Dimension
myBorderWidth = 2

myTerminal :: String
myTerminal = "urxvt"

main :: IO ()
main = do
  xmonad $ def {
    borderWidth = myBorderWidth
    , terminal = myTerminal
  }
