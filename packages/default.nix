 [
   (self: super:
     with super;
     let tmuxPlugins = (callPackage (import ./tmux-plugins.nix) { });
     in {
       my = {
         tmuxPlugins.per-project-session = tmuxPlugins.per-project-session;
       };

       unstable = import <nixos-unstable> { inherit config; };
     })

   (import (builtins.fetchTarball {
     url =
       "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
   }))
 ]
