module Main (main) where

import           Control.Monad
import qualified Data.Map                         as M
import           Data.Monoid
import           System.Exit
import           System.IO
import           XMonad
import           XMonad.Actions.WindowNavigation
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Layout.Gaps
import           XMonad.Layout.IndependentScreens
import           XMonad.Layout.LayoutModifier
import           XMonad.Layout.Spacing
import           XMonad.Layout.ThreeColumns
import           XMonad.Layout.WindowNavigation
import qualified XMonad.StackSet                  as W
import           XMonad.Util.Run

-- xmonad.hs
-- author: Seong Yong-ju <sei40kr@gmail.com>

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal :: String
myTerminal = "termite"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True
-- Width of the window border in pixels.
--
myBorderWidth   :: Dimension
myBorderWidth   = 3

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       :: KeyMask
myModMask       = mod1Mask

-- NOTE: from 0.9.1 on numlock mask is set automatically. The numlockMask
-- setting should be removed from configs.
--
-- You can safely remove this even on earlier xmonad versions unless you
-- need to set it to something other than the default mod2Mask, (e.g. OSX).
--
-- The mask for the numlock key. Numlock status is "masked" from the
-- current modifier status, so the keybindings will work with numlock on or
-- off. You may need to change this on some systems.
--
-- You can find the numlock modifier by running "xmodmap" and looking for a
-- modifier with Num_Lock bound to it:
--
-- > $ xmodmap | grep Num
-- > mod2        Num_Lock (0x4d)
--
-- Set numlockMask = 0 if you don't have a numlock key, or want to treat
-- numlock status separately.
--
-- myNumlockMask   = mod2Mask -- deprecated in xmonad-0.9.1
------------------------------------------------------------


-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
devWs :: String
devWs = "dev"
webWs :: String
webWs = "web"
fileWs :: String
fileWs = "file"
imWs :: String
imWs = "im"

myWorkspaces :: [String]
myWorkspaces = [devWs, webWs, fileWs, imWs]


------------------------------------------------------------------------
-- Colors:
--

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor :: String
myNormalBorderColor = "#000000"
myFocusedBorderColor :: String
myFocusedBorderColor = "#0088cc"

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@XConfig {XMonad.modMask = modm} =
  M.fromList $
    -- launch rofi
  [ ((mod4Mask, xK_space), spawn "rofi -show drun")
    -- close focused window
  , ((mod4Mask, xK_q), kill)
     -- Rotate through the available layout algorithms
  , ((modm .|. shiftMask, xK_space), sendMessage NextLayout)
    --  Reset the layouts on the current workspace to default
  , ( (modm .|. controlMask .|. shiftMask, xK_space)
    , setLayout $ XMonad.layoutHook conf)
    -- Move focus to the next window
  , ((mod4Mask .|. shiftMask, xK_Tab), windows W.focusDown)
    -- Move focus to the next window
  , ((modm .|. shiftMask, xK_j), windows W.focusDown)
    -- Move focus to the previous window
  , ((modm .|. shiftMask, xK_k), windows W.focusUp)
    -- Move focus to the master window
  , ((modm .|. shiftMask, xK_m), windows W.focusMaster)
    -- Swap the focused window and the master window
  , ((modm .|. shiftMask, xK_Return), windows W.swapMaster)
    -- Swap the focused window with the next window
  , ((modm .|. controlMask .|. shiftMask, xK_j), windows W.swapDown)
    -- Swap the focused window with the previous window
  , ((modm .|. controlMask .|. shiftMask, xK_k), windows W.swapUp)
    -- Shrink the master area
  , ((modm .|. shiftMask, xK_h), sendMessage Shrink)
    -- Expand the master area
  , ((modm .|. shiftMask, xK_l), sendMessage Expand)
    -- TODO Bind mod+shift+t to toggle whether or not the focused window is floating
    -- Push window back into tiling
    -- , ((modm, xK_t), withFocused $ windows . W.sink)
    -- Increment the number of windows in the master area
  , ((modm .|. shiftMask, xK_comma), sendMessage (IncMasterN 1))
    -- Deincrement the number of windows in the master area
  , ((modm .|. shiftMask, xK_period), sendMessage (IncMasterN (-1)))
    -- Restart xmonad
  , ((modm .|. shiftMask, xK_q), spawn "xmonad --recompile; xmonad --restart")
    -- Capture a screenshot
  , ((mod4Mask .|. shiftMask, xK_3), spawn "scrot '%Y-%m-%d-%T-screenshot.png'")
    -- Capture a screenshot selecting a window or rectangle with the mouse
  , ( (mod4Mask .|. shiftMask, xK_4)
    , spawn "scrot -s '%Y-%m-%d-%T-screenshot.png'")
  ] ++
    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
  [ ((modm .|. shiftMask .|. m, k), windows $ f i)
  | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
  , (f, m) <- [(W.greedyView, 0), (W.shift, controlMask)]
  ] ++
    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
  [ ((modm .|. shiftMask .|. m, key), screenWorkspace sc >>= flip whenJust (windows . f))
  | (key, sc) <- zip [xK_w, xK_e, xK_r] [0 ..]
  , (f, m) <- [(W.view, 0), (liftM2 (.) W.view W.shift, controlMask)]
  ]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings :: XConfig l -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings XConfig {XMonad.modMask = modm} = M.fromList

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster)

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), \w -> focus w >> windows W.shiftMaster)

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), \w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster)

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- * NOTE: XMonad.Hooks.EwmhDesktops users must remove the obsolete
-- ewmhDesktopsLayout modifier from layoutHook. It no longer exists.
-- Instead use the 'ewmh' function from that module to modify your
-- defaultConfig as a whole. (See also logHook, handleEventHook, and
-- startupHook ewmh notes.)
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout ::
     ModifiedLayout AvoidStruts (ModifiedLayout SmartSpacing (Choose (ModifiedLayout Gaps (ModifiedLayout WindowNavigation ThreeCol)) Full)) Window
myLayout =
  avoidStruts $
  smartSpacing 16 $
  gaps [(U, 16), (D, 16), (R, 16), (L, 16)] (windowNavigation tiled) ||| Full
    -- default tiling algorithm partitions the screen into two panes
  where
    tiled = ThreeColMid nmaster delta ratio
    -- The default number of windows in the master pane
    nmaster = 1
    -- Default proportion of screen occupied by master pane
    ratio = toRational (2 / (1 + sqrt 5 :: Double))
    -- Percent of screen to increment by when resizing panes
    delta = 3 / 100

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook :: ManageHook
myManageHook =
  composeAll
    [ resource =? "desktop_window" --> doIgnore
    , className =? "Discord" --> doShiftAndView imWs
    , className =? "Emacs" --> doShiftAndView devWs
    , className =? "Fcitx-config-gtk3" --> doCenterFloat
    , className =? "feh" --> doCenterFloat
    , className =? "Gitter" --> doShiftAndView imWs
    , className =? "Google-chrome" --> doShiftAndView webWs
    , className =? "Rofi" --> doCenterFloat
    , className =? "Skype" --> doShiftAndView imWs
    , className =? "Slack" --> doShiftAndView imWs
    , className =? "Thunar" --> doShiftAndView fileWs
    , className =? "Transmission-gtk" --> doShiftAndView fileWs
    , className =? "vlc" --> doShiftAndView fileWs
    , className =? "Zeal" --> doShiftAndView devWs <+> doCenterFloat
    ]
  where
    doShiftAndView = doF . liftM2 (.) W.view W.shift

-----------------------------------------------------------------------
-- Event handling

-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
-- * NOTE: EwmhDesktops users should use the 'ewmh' function from
-- XMonad.Hooks.EwmhDesktops to modify their defaultConfig as a whole.
-- It will add EWMH event handling to your custom event hooks by
-- combining them with ewmhDesktopsEventHook.
--
myEventHook :: Event -> X Data.Monoid.All
myEventHook = mempty

------------------------------------------------------------------------
-- Status bars and logging

xmobarFont :: Int -> String -> String
xmobarFont n = wrap ("<fn="  ++ show n ++ ">") "</fn>"

xmobarWs :: String -> String
xmobarWs ws | ws == devWs = "\62601"
xmobarWs ws | ws == webWs = "\62596"
xmobarWs ws | ws == fileWs = "\62483"
xmobarWs ws | ws == imWs = "\63593"
xmobarWs ws = ws

xmobarWsFont :: Int
xmobarWsFont = 3
xmobarActiveWsColor :: String
xmobarActiveWsColor = "#ffffff"
xmobarInactiveWsColor :: String
xmobarInactiveWsColor = "#999999"
xmobarWsSep :: String
xmobarWsSep = "    "

xmobarSep :: String
xmobarSep = "  |  "
xmobarSepColor :: String
xmobarSepColor = "#555555"
xmobarSepFont :: Int
xmobarSepFont = 2

xmobarTitleLength :: Int
xmobarTitleLength = 96
xmobarTitleColor :: String
xmobarTitleColor = "#ffffff"

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
--
-- * NOTE: EwmhDesktops users should use the 'ewmh' function from
-- XMonad.Hooks.EwmhDesktops to modify their defaultConfig as a whole.
-- It will add EWMH logHook actions to your custom log hook by
-- combining it with ewmhDesktopsLogHook.
--
myLogHook :: [Handle] -> X ()
myLogHook hs =
  dynamicLogWithPP
    xmobarPP
      { ppCurrent =
          xmobarColor xmobarActiveWsColor "" .
          xmobarFont xmobarWsFont . xmobarWs
      , ppVisible =
          xmobarColor xmobarInactiveWsColor "" .
          xmobarFont xmobarWsFont . xmobarWs
      , ppHidden =
          xmobarColor xmobarInactiveWsColor "" .
          xmobarFont xmobarWsFont . xmobarWs
      , ppUrgent =
          xmobarColor xmobarInactiveWsColor "" .
          xmobarFont xmobarWsFont . xmobarWs
      , ppWsSep = xmobarWsSep
      , ppSep =
          xmobarColor xmobarSepColor "" $ xmobarFont xmobarSepFont xmobarSep
      , ppTitle = xmobarColor xmobarTitleColor "" . shorten xmobarTitleLength
      , ppTitleSanitize = xmobarStrip
      , ppOrder = \(ws:_:t:_) -> [ws, t]
      , ppOutput = forM_ hs . flip hPutStrLn
      }


------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
--
-- * NOTE: EwmhDesktops users should use the 'ewmh' function from
-- XMonad.Hooks.EwmhDesktops to modify their defaultConfig as a whole. It will
-- add initialization of EWMH support to your custom startup hook by combining
-- it with ewmhDesktopsStartup.
--
myStartupHook :: X ()
myStartupHook = return ()

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main :: IO ()
main = do
  screens <- countScreens :: IO Integer
  hs <- mapM (spawnPipe . ("xmobar -x" ++) . show . (-1 +)) [1 .. screens]
  xmonad $ defaults hs

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
defaults hs =
  ewmh $
  docks $
  def
      -- simple stuff
    { terminal = myTerminal
    , focusFollowsMouse = myFocusFollowsMouse
    , borderWidth = myBorderWidth
    , modMask = myModMask
    , workspaces = myWorkspaces
    , normalBorderColor = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
      -- key bindings
    , keys = myKeys
    , mouseBindings = myMouseBindings
      -- hooks, layouts
    , layoutHook = myLayout
    , manageHook = myManageHook
    , handleEventHook = myEventHook
    , logHook = myLogHook hs
    , startupHook = myStartupHook
    }
