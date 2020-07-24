 [
   (self: super:
     with super;
     let
       unstable = import <nixos-unstable> { inherit config; };
       python3Packages = (callPackage (import ./python3-packages.nix) { });
       tmuxPlugins = (callPackage (import ./tmux-plugins.nix) { });
       vimPlugins = (callPackage (import ./vim-plugins.nix) { });
       vscode-extensions = (callPackage (import ./vscode-extensions.nix) { });
     in {
       inherit unstable;

       my = {
         inherit python3Packages tmuxPlugins vimPlugins vscode-extensions;

         kotlin-language-server = callPackage ./kotlin-language-server.nix {
           gradle_6 = unstable.gradle;
         };
         zinit = callPackage ./zinit.nix { };
       };
     })

   (import (builtins.fetchTarball {
     url =
       "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
   }))
 ]
