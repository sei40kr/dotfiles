{ lib, ... }:

with lib; {
  imports = [
    ./apps
    ./backends
    ./browsers
    ./config
    ./i18n
    ./tools
    ./x11
    ./fonts.nix
    ./xdg-user-dirs.nix
    ./xmonad.nix
    ./xsecurelock.nix
  ];
}
