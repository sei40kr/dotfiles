{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [ ./deluge.nix ./rofi.nix ./slack.nix ./thunar.nix ];
}
