{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [
    ./apps
    ./backends
    ./browsers
    ./config
    ./term
    ./i18n
    ./tools
    ./x11
    ./fonts.nix
    ./xdg-user-dirs.nix
    ./xmonad.nix
    ./xsecurelock.nix
  ];
}
