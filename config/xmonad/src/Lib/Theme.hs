module Lib.Theme
  ( myBorderWidth
  , myNormalBorderColor
  , myFocusedBorderColor
  , tabTheme
  )
where
import           XMonad
import           XMonad.Layout.Tabbed

-- Width of the window border in pixels.
--
myBorderWidth :: Dimension
myBorderWidth = 0

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor :: String
myNormalBorderColor = ""
myFocusedBorderColor :: String
myFocusedBorderColor = ""

tabTheme = def { activeColor         = ""
               , activeBorderColor   = ""
               , activeTextColor     = ""
               , inactiveColor       = ""
               , inactiveBorderColor = ""
               , inactiveTextColor   = ""
               , urgentColor         = ""
               , urgentBorderColor   = ""
               , urgentTextColor     = ""
               , fontName            = ""
               , decoHeight          = 0
               }
