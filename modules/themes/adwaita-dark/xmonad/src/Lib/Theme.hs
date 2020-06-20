module Lib.Theme
  ( myNormalBorderColor
  , myFocusedBorderColor
  , tabTheme
  )
where
import           XMonad
import           XMonad.Layout.Tabbed

-- Width of the window border in pixels.
--
myBorderWidth :: Dimension
myBorderWidth = 1

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor :: String
myNormalBorderColor = "#ffffff"
myFocusedBorderColor :: String
myFocusedBorderColor = "#9a2223"

tabTheme = def { activeColor         = "#9a2223"
               , activeBorderColor   = "#1c1c1c"
               , activeTextColor     = "#ffffff"
               , inactiveColor       = "#202124"
               , inactiveBorderColor = "#1c1c1c"
               , inactiveTextColor   = "#cccccc"
               , urgentColor         = "#202124"
               , urgentBorderColor   = "#1c1c1c"
               , urgentTextColor     = "#cccccc"
               , fontName            = "xft:Noto Sans CJK JP:size=10"
               , decoHeight          = 32
               }
