{ pkgs, tmux-project, ... }:

rec {
  tmuxPlugins = (pkgs.callPackage (import ./tmux-plugins) { });

  vimPlugins = pkgs.callPackage ./vim-plugins { };

  amphetamine-enhancer = pkgs.callPackage ./amphetamine-enhancer { };

  corretto_11 = pkgs.callPackage ./corretto_11.nix { };

  online-judge-verify-helper =
    pkgs.python3Packages.callPackage ./online-judge-verify-helper { };

  qbittorrent-ee = pkgs.callPackage ./qbittorrent-ee { };

  sensible-browser = pkgs.callPackage ./sensible-browser { };

  sensible-terminal = pkgs.callPackage ./sensible-terminal { };

  whitesur-cursors = pkgs.callPackage ./whitesur-cursors { };

  whitesur-kde = pkgs.callPackage ./whitesur-kde { };

  whitesur-wallpapers = pkgs.callPackage ./whitesur-wallpapers { };

  yonmux = pkgs.callPackage ./yonmux {
    inherit tmux-project;
    tmuxPlugins = pkgs.tmuxPlugins // tmuxPlugins;
  };

  zsh-smart-history = pkgs.callPackage ./zsh-smart-history.nix { };

  zsh-tmux-man = pkgs.callPackage ./zsh-tmux-man.nix { };
}
