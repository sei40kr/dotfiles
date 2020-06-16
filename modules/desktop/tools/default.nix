{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [ ./random-background.nix ./scrot.nix ];
}
