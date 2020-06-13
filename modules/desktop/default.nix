{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [
    ./apps
    ./term
    ./i18n
    ./tools
    ./dunst.nix
    ./fonts.nix
    ./picom.nix
    ./polybar.nix
    ./xdg-user-dirs.nix
    ./xmonad.nix
    ./xsecurelock.nix
  ];
}
