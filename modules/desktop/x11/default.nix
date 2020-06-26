{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [ ./xbindkeys.nix ];
}
