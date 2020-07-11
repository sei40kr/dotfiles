 [
   (self: super:
     with super;
     let
       tmuxPlugins = (callPackage (import ./tmux-plugins.nix) { });
       vscode-extensions = (callPackage (import ./vscode-extensions.nix) { });
     in {
       my = { inherit tmuxPlugins vscode-extensions; };

       unstable = import <nixos-unstable> { inherit config; };
     })

   (import (builtins.fetchTarball {
     url =
       "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
   }))
 ]
