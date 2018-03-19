import XMonad

myTerminal :: String
myTerminal = "urxvtc"

main :: IO ()
main = do
  xmonad $ def {
    terminal = myTerminal
  }
