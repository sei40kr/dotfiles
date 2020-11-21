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
myFocusedBorderColor = "#0f6776"

tabTheme = def { activeBorderColor   = "#0f6776"
               , activeColor         = "#0f6776"
               , activeTextColor     = "#ffffff"
               , inactiveBorderColor = "#202124"
               , inactiveColor       = "#202124"
               , inactiveTextColor   = "#cccccc"
               , urgentBorderColor   = "#202124"
               , urgentColor         = "#202124"
               , urgentTextColor     = "#cccccc"
               , fontName            = "xft:Noto Sans CJK JP:size=10"
               , decoHeight          = 36
               }
