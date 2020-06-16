{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [
    ./media
    ./web-browsers
    ./deluge.nix
    ./dunst.nix
    ./geary.nix
    ./gnome-calendar.nix
    ./gnome-control-center.nix
    ./polybar.nix
    ./rofi.nix
    ./seahorse.nix
    ./slack.nix
    ./thunar.nix
  ];
}
