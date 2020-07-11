 [
   (self: super:
     with super;
     let
       python3Packages = (callPackage (import ./python3-packages.nix) { });
       tmuxPlugins = (callPackage (import ./tmux-plugins.nix) { });
       vscode-extensions = (callPackage (import ./vscode-extensions.nix) { });
     in {
       my = { inherit python3Packages tmuxPlugins vscode-extensions; };

       unstable = import <nixos-unstable> { inherit config; };
     })

   (import (builtins.fetchTarball {
     url =
       "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
   }))
 ]
