{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [ ./picom.nix ./psd.nix ./scrot.nix ];
}
