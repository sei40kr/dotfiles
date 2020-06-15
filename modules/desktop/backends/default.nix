{ config, lib, pkgs, ... }:

with lib; {
  imports = [ ./dconf.nix ];
}
