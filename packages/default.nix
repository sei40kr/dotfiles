{
  callPackage,
  python3Packages,
  tmux-project,
  tmuxPlugins,
  ...
}:

let
  myTmuxPlugins = callPackage ./tmux-plugins { };
in
{
  tmuxPlugins = myTmuxPlugins;

  vimPlugins = callPackage ./vim-plugins { };

  online-judge-verify-helper = python3Packages.callPackage ./online-judge-verify-helper { };

  whitesur-kde = callPackage ./whitesur-kde { };

  whitesur-wallpapers = callPackage ./whitesur-wallpapers { };

  yonmux = callPackage ./yonmux {
    inherit tmux-project;
    tmuxPlugins = tmuxPlugins // myTmuxPlugins;
  };

  zsh-smart-history = callPackage ./zsh-smart-history.nix { };

  zsh-tmux-man = callPackage ./zsh-tmux-man.nix { };
}
