import XMonad

myTerminal :: String
myTerminal = "urxvt"

main :: IO ()
main = do
  xmonad $ def {
    terminal = myTerminal
  }
