{
  callPackage,
  python3Packages,
  ...
}:

{
  vimPlugins = callPackage ./vim-plugins { };

  julia-mono-nf = callPackage ./julia-mono-nf { };

  online-judge-verify-helper = python3Packages.callPackage ./online-judge-verify-helper { };

  qml-niri = callPackage ./qml-niri { };

  whitesur-kde = callPackage ./whitesur-kde { };

  whitesur-wallpapers = callPackage ./whitesur-wallpapers { };

  zsh-smart-history = callPackage ./zsh-smart-history.nix { };
}
