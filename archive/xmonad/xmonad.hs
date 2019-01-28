{-# LANGUAGE MultiWayIf #-}

module Main (main) where

import           Control.Monad
import qualified Data.Map                         as M
import           Data.Monoid
import           Data.Ratio                       ((%))
import           System.Exit
import           System.IO
import           XMonad
import           XMonad.Actions.OnScreen
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Layout.IndependentScreens
import           XMonad.Layout.LayoutModifier
import           XMonad.Layout.PerWorkspace
import           XMonad.Layout.Tabbed
import           XMonad.Layout.ZoomRow
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
myBorderWidth = 2

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       :: KeyMask
myModMask       = mod1Mask .|. shiftMask

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
termWorkspace :: String
termWorkspace = "term"
codeWorkspace :: String
codeWorkspace = "code"
webWorkspace :: String
webWorkspace = "web"
fileWorkspace :: String
fileWorkspace = "file"
imWorkspace :: String
imWorkspace = "im"

myWorkspaces :: [String]
myWorkspaces = [termWorkspace, codeWorkspace, webWorkspace, fileWorkspace, imWorkspace]

------------------------------------------------------------------------
-- Colors:
--

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor :: String
myNormalBorderColor = "#555555"
myFocusedBorderColor :: String
myFocusedBorderColor = "#ffffff"

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
  , ((modm, xK_space), sendMessage NextLayout)
    --  Reset the layouts on the current workspace to default
  , ((modm .|. controlMask, xK_space), setLayout $ XMonad.layoutHook conf)
    -- Move focus to the next window
  , ((mod4Mask .|. shiftMask, xK_Tab), windows W.focusDown)
    -- Move focus to the next window
  , ((modm, xK_j), windows W.focusDown)
    -- Move focus to the previous window
  , ((modm, xK_k), windows W.focusUp)
    -- Move focus to the master window
  , ((modm, xK_m), windows W.focusMaster)
    -- Swap the focused window and the master window
  , ((modm, xK_Return), windows W.swapMaster)
    -- Swap the focused window with the next window
  , ((modm .|. controlMask, xK_j), windows W.swapDown)
    -- Swap the focused window with the previous window
  , ((modm .|. controlMask, xK_k), windows W.swapUp)
    -- Shrink the master area
  , ((modm, xK_h), sendMessage Shrink)
    -- Expand the master area
  , ((modm, xK_l), sendMessage Expand)
    -- Push window back into tiling
    -- , ((modm, xK_t), withFocused $ windows . W.sink)
    -- Increment the number of windows in the master area
  , ((modm, xK_comma), sendMessage (IncMasterN 1))
    -- Deincrement the number of windows in the master area
  , ((modm, xK_period), sendMessage (IncMasterN (-1)))
    -- Restart xmonad
  , ((modm, xK_q), spawn "xmonad --recompile; xmonad --restart")
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
  [ ((modm .|. m, k), windows $ f i)
  | (i, k) <-
      zip (XMonad.workspaces conf) $ take (length myWorkspaces) [xK_1 .. xK_9]
  , (f, m) <- [(greedyViewOnSpecifiedScreen, 0)]
  ] ++
    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
  [ ((modm .|. m, key), screenWorkspace sc >>= flip whenJust (windows . f))
  | (key, sc) <- zip [xK_w, xK_e, xK_r] [0 ..]
  , (f, m) <- [(W.view, 0)]
  ]
  where
    greedyViewOnSpecifiedScreen workspaceId =
      let screen = specifiedScreen workspaceId
       in (case screen of
             Just screenId -> greedyViewOnScreen screenId
             Nothing       -> W.greedyView)
            workspaceId
    specifiedScreen workspaceId =
      (if | workspaceId == termWorkspace -> Just 0
          | workspaceId == codeWorkspace -> Just 0
          | workspaceId == webWorkspace -> Just 1
          | workspaceId == fileWorkspace -> Just 0
          | workspaceId == imWorkspace -> Just 1
          | otherwise -> Nothing)


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
myLayout =
  avoidStruts $
  onWorkspace termWorkspace (myTabbed ||| myTall) $
  onWorkspace codeWorkspace (myTabbed ||| myTall) $
  onWorkspace fileWorkspace (myRow ||| myTabbed) $
  onWorkspace webWorkspace myTabbed $
  onWorkspace imWorkspace myTabbed
  Full
    -- default tiling algorithm partitions the screen into two panes
  where
    myTheme =
      def
        { activeColor = "#111"
        , activeBorderColor = "#111"
        , activeTextColor = "#fff"
        , inactiveColor = "#111"
        , inactiveBorderColor = "#111"
        , inactiveTextColor = "#999"
        , urgentColor = "#111"
        , urgentBorderColor = "#111"
        , urgentTextColor = "#999"
        , fontName = "xft:Source Han Code JP:pixelsize=14"
        , decoHeight = 32
        }
    myTabbed = tabbed shrinkText myTheme
    myTall = Tall nmaster delta ratio
    myRow = zoomRow
   -- The default number of windows in the master pane
    nmaster = 1
    -- Default proportion of screen occupied by master pane
    ratio = toRational $ 2 / (1 + sqrt 5 :: Double)
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
    , className =? "Fcitx-config-gtk3" --> doCenterFloat
    , className =? "feh" --> doCenterFloat
    , isDialog --> doCenterFloat
    , className =? "Rofi" --> doCenterFloat
    , className =? "Termite" --> doShiftAndView termWorkspace
    , className =? "Emacs" --> doShiftAndView codeWorkspace
    , className =? "Zeal" --> doShiftAndView codeWorkspace <+> doCenterFloat
    , stringProperty "WM_WINDOW_ROLE" =? "browser" -->
      doShiftAndView webWorkspace
    , className =? "Transmission-gtk" --> doShiftAndView webWorkspace
    , className =? "Thunar" --> doShiftAndView fileWorkspace
    , className =? "vlc" --> doShiftAndView fileWorkspace
    , className =? "Skype" --> doShiftAndView imWorkspace
    , className =? "Slack" --> doShiftAndView imWorkspace
    , className =? "Gitter" --> doShiftAndView imWorkspace
    , className =? "Discord" --> doShiftAndView imWorkspace
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
      { ppCurrent = xmobarColor activeWsColor "" . transformWs
      , ppVisible = xmobarColor activeWsColor "" . transformWs
      , ppHidden = xmobarColor inactiveWsColor "" . transformWs
      , ppHiddenNoWindows = xmobarColor inactiveWsColor "" . transformWs
      , ppUrgent = xmobarColor inactiveWsColor "" . transformWs
      , ppWsSep = spacerL
      , ppSep =
          wrap spacerM spacerM $ xmobarFont 2 $ xmobarColor "#555555" "" "|"
      , ppOrder = \(ws:_:_:_) -> [ws]
      , ppOutput = forM_ hs . flip hPutStrLn
      }
  where
    activeWsColor = "#ffffff"
    inactiveWsColor = "#999999"
    spacerS = xmobarFont 1 " "
    spacerM = xmobarFont 1 "  "
    spacerL = xmobarFont 1 "   "
    transformWs ws =
      let withIcon icon ws = (xmobarFont 3 icon) ++ spacerM ++ ws
          withAction i =
            wrap ("<action=xdotool key alt+shift+" ++ show i ++ ">") "</action>"
       in if | ws == termWorkspace -> withAction 1 $ withIcon "\62601" "1"
             | ws == codeWorkspace -> withAction 2 $ withIcon "\62543" "2"
             | ws == webWorkspace -> withAction 3 $ withIcon "\62596" "3"
             | ws == fileWorkspace -> withAction 4 $ withIcon "\62483" "4"
             | ws == imWorkspace -> withAction 5 $ withIcon "\62495" "5"
             | otherwise -> ""

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
