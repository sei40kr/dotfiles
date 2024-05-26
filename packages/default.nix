{ callPackage, python3Packages, tmux-project, tmuxPlugins, ... }:

let
  myTmuxPlugins = callPackage ./tmux-plugins { };
in
{
  tmuxPlugins = myTmuxPlugins;

  vimPlugins = callPackage ./vim-plugins { };

  amphetamine-enhancer = callPackage ./amphetamine-enhancer { };

  corretto_11 = callPackage ./corretto_11.nix { };

  online-judge-verify-helper = python3Packages.callPackage ./online-judge-verify-helper { };

  sensible-browser = callPackage ./sensible-browser { };

  sensible-terminal = callPackage ./sensible-terminal { };

  spark-desktop = callPackage ./spark-desktop { };

  whitesur-kde = callPackage ./whitesur-kde { };

  whitesur-wallpapers = callPackage ./whitesur-wallpapers { };

  yonmux = callPackage ./yonmux {
    inherit tmux-project;
    tmuxPlugins = tmuxPlugins // myTmuxPlugins;
  };

  zsh-smart-history = callPackage ./zsh-smart-history.nix { };

  zsh-tmux-man = callPackage ./zsh-tmux-man.nix { };
}
