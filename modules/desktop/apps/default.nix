{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [ ./web-browsers ./deluge.nix ./rofi.nix ./slack.nix ./thunar.nix ];
}
