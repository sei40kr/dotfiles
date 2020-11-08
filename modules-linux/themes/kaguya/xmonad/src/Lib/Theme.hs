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
myBorderWidth = 2

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor :: String
myNormalBorderColor = "#ffffff"
myFocusedBorderColor :: String
myFocusedBorderColor = "#9a2223"

tabTheme = def { activeColor         = "#9a2223"
               , activeBorderWidth   = 0
               , activeTextColor     = "#ffffff"
               , inactiveColor       = "#202124"
               , inactiveBorderWidth = 0
               , inactiveTextColor   = "#cccccc"
               , urgentColor         = "#202124"
               , urgentBorderWidth   = 0
               , urgentTextColor     = "#cccccc"
               , fontName            = "xft:Noto Sans CJK JP:size=10"
               , decoHeight          = 36
               }
