{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [ ./psd.nix ./scrot.nix ];
}
