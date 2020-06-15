{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [
    ./media
    ./web-browsers
    ./deluge.nix
    ./dunst.nix
    ./geary.nix
    ./polybar.nix
    ./rofi.nix
    ./slack.nix
    ./thunar.nix
  ];
}
