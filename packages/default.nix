{ pkgs, ... }:

rec {
  tmuxPlugins = (pkgs.callPackage (import ./tmux-plugins) { });

  vimPlugins = pkgs.callPackage ./vim-plugins { };

  alfred = pkgs.callPackage ./alfred.nix { };

  corretto_11 = pkgs.callPackage ./corretto_11.nix { };

  dash = pkgs.callPackage ./dash.nix { };

  dataspell = pkgs.callPackage ./dataspell { inherit (pkgs.jetbrains) jdk; };

  kotlin-language-server = pkgs.callPackage ./kotlin-language-server.nix { };

  online-judge-template-generator =
    pkgs.python3Packages.callPackage ./online-judge-template-generator { };

  online-judge-verify-helper =
    pkgs.python3Packages.callPackage ./online-judge-verify-helper { };

  notion = pkgs.callPackage ./notion.nix { };

  qbittorrent-ee = pkgs.callPackage ./qbittorrent-ee { };

  sensible-browser = pkgs.callPackage ./sensible-browser { };

  video-trimmer = pkgs.callPackage ./video-trimmer { };

  whitesur-cursors = pkgs.callPackage ./whitesur-cursors { };

  whitesur-dark-ulauncher = pkgs.callPackage ./whitesur-dark-ulauncher { };

  whitesur-light-ulauncher = pkgs.callPackage ./whitesur-light-ulauncher { };

  whitesur-wallpapers = pkgs.callPackage ./whitesur-wallpapers { };

  yonmux = pkgs.callPackage ./yonmux { tmuxPlugins = pkgs.tmuxPlugins // tmuxPlugins; };

  zi = pkgs.callPackage ./zi { };

  zsh-smart-history = pkgs.callPackage ./zsh-smart-history.nix { };

  zsh-tmux-man = pkgs.callPackage ./zsh-tmux-man.nix { };
}
