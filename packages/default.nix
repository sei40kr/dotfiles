{ pkgs, ... }:

rec {
  tmuxPlugins = (pkgs.callPackage (import ./tmux-plugins) { });

  vimPlugins = pkgs.callPackage ./vim-plugins { };

  alfred = pkgs.callPackage ./alfred.nix { };

  corretto_11 = pkgs.callPackage ./corretto_11.nix { };

  dash = pkgs.callPackage ./dash.nix { };

  dataspell = pkgs.callPackage ./dataspell { inherit (pkgs.jetbrains) jdk; };

  kotlin-language-server = pkgs.callPackage ./kotlin-language-server.nix { };

  nwg-bar = pkgs.callPackage ./nwg-bar { };

  online-judge-template-generator =
    pkgs.python3Packages.callPackage ./online-judge-template-generator { };

  online-judge-verify-helper =
    pkgs.python3Packages.callPackage ./online-judge-verify-helper { };

  notion = pkgs.callPackage ./notion.nix { };

  zsh-smart-history = pkgs.callPackage ./zsh-smart-history.nix { };

  zsh-tmux-man = pkgs.callPackage ./zsh-tmux-man.nix { };
}
