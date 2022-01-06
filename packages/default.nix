{ pkgs, ... }:

rec {
  python3Packages =
    (pkgs.python3Packages.callPackage (import ./python3-packages.nix) { });

  tmuxPlugins = (pkgs.callPackage (import ./tmux-plugins) { });

  vimPlugins = pkgs.callPackage ./vim-plugins { };

  alfred = pkgs.callPackage ./alfred.nix { };

  corretto_11 = pkgs.callPackage ./corretto_11.nix { };

  dash = pkgs.callPackage ./dash.nix { };

  dataspell = pkgs.callPackage ./dataspell { inherit (pkgs.jetbrains) jdk; };

  kotlin-language-server = pkgs.callPackage ./kotlin-language-server.nix { };

  online-judge-tools =
    pkgs.python3Packages.toPythonApplication python3Packages.online-judge-tools;

  online-judge-template-generator =
    pkgs.python3Packages.callPackage ./online-judge-template-generator {
      inherit (python3Packages) online-judge-api-client online-judge-tools;
    };

  online-judge-verify-helper =
    pkgs.python3Packages.callPackage ./online-judge-verify-helper {
      inherit (python3Packages) importlab online-judge-tools;
    };

  notion = pkgs.callPackage ./notion.nix { };

  notion-app-enhanced = pkgs.callPackage ./notion-app-enhanced { };

  zsh-smart-history = pkgs.callPackage ./zsh-smart-history.nix { };

  zsh-tmux-man = pkgs.callPackage ./zsh-tmux-man.nix { };
}
