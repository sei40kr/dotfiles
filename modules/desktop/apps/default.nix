{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [ ./deluge.nix ./rofi.nix ./thunar.nix ];
}
