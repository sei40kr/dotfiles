{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [ ./picom.nix ./psd.nix ./random-background.nix ./scrot.nix ];
}
