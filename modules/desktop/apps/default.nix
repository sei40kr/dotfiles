{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [ ./rofi.nix ];
}
