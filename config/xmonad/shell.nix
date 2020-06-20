{ ghc }:
with (import <nixpkgs> { });

haskell.lib.buildStackProject {
  inherit ghc;
  name = "my-xmonad";
  buildInputs = [
    gcc
    xorg.libX11
    xorg.libXScrnSaver
    xorg.libXext
    xorg.libXft
    xorg.libXinerama
    xorg.libXrandr
  ];
}
