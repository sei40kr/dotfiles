      [
        (_: super:
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

              alfred = callPackage ./alfred.nix { };
              corretto_11 = callPackage ./corretto_11.nix { };
              delta = callPackage ./delta.nix { };
              emacs = callPackage ./emacs.nix { emacs = unstable.emacs; };
              haskell-language-server = callPackage ./haskell-language-server.nix { };
              jenv = callPackage ./jenv.nix { };
              kotlin-language-server = callPackage ./kotlin-language-server.nix {
                gradle_6 = unstable.gradle;
              };
              notion = callPackage ./notion.nix { };
              operator-mono = callPackage ./operator-mono.nix { };
              zinit = callPackage ./zinit.nix { };
            };
          })
      ]
