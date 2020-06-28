module Lib.Actions
  ( spawnPolybar
  , spawnRofi
  , spawnStartup
  )
where
import           XMonad

spawnPolybar :: X ()
spawnPolybar = return ()

spawnRofi :: [String] -> X ()
spawnRofi _ = return ()

spawnStartup :: X ()
spawnStartup = spawnPolybar
