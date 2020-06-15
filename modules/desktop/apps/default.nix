{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [
    ./media
    ./web-browsers
    ./deluge.nix
    ./geary.nix
    ./rofi.nix
    ./slack.nix
    ./thunar.nix
  ];
}
