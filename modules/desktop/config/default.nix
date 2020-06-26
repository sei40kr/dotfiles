{ config, lib, pkgs, ... }:

with lib; {
  imports = [ ./gtk.nix ];
}
