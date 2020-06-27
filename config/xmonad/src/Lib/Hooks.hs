module Lib.Hooks
  ( myStartupHook
  )
where
import           XMonad
import           XMonad.Hooks.EwmhDesktops

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.

myStartupHook = ewmhDesktopsStartup
