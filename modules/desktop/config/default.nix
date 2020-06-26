{ config, lib, pkgs, ... }:

with lib; {
  imports = [ ./fontconfig.nix ./gtk.nix ];
}
